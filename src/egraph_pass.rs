//! Main egraph pass implementation
//!
//! This pass does three main things:
//! 1. Removes pure operations from the layout (CFG)
//! 2. Builds an egraph with GVN and applies rewrite rules
//! 3. Elaborates (extracts) the best version back into the CFG

use crate::pattern::*;
use crate::range::{learn_from_comparison, Range, RangeAssumptions};
use crate::rewrite_integration::*;
use crate::support::*;
use crate::types::*;
use std::collections::{HashMap, HashSet};

const REWRITE_LIMIT: usize = 5;

/// Main egraph pass structure
pub struct EgraphPass {
    /// The function's data flow graph
    pub dfg: DataFlowGraph,

    /// The control flow layout
    pub layout: Layout,

    /// Dominator tree for the CFG
    pub domtree: DominatorTree,

    /// Statistics collected during the pass
    pub stats: Stats,

    /// Pattern-based rewrite engine
    rewrite_engine: RewriteEngine,

    /// GVN mapping from original values to optimized values
    gvn_mapping: HashMap<ValueId, ValueId>,

    pub range_assumptions: RangeAssumptions,

    /// Range facts per CFG edge (pred, target), populated during
    /// `remove_pure_and_optimize` and reused by the elaborator's domtree walk.
    pub block_entry_facts: HashMap<(BlockId, BlockId), Vec<(ValueId, crate::range::Range)>>,

    /// CFG predecessor map, populated during `remove_pure_and_optimize`.
    pub cfg_preds: HashMap<BlockId, Vec<BlockId>>,

    pub path_sensitive: bool,
}

impl EgraphPass {
    pub fn new(dfg: DataFlowGraph, layout: Layout, domtree: DominatorTree) -> Self {
        Self {
            dfg,
            layout,
            domtree,
            stats: Stats::default(),
            rewrite_engine: RewriteEngine::with_standard_library(),
            gvn_mapping: HashMap::new(),
            range_assumptions: RangeAssumptions::new(),
            block_entry_facts: HashMap::new(),
            cfg_preds: HashMap::new(),
            path_sensitive: true,
        }
    }

    pub fn set_path_sensitive(&mut self, enabled: bool) {
        self.path_sensitive = enabled;
    }

    pub fn assume_range(&mut self, value: ValueId, min: i64, max: i64) {
        self.range_assumptions
            .assume_range(value, crate::range::Range::new(min, max));
    }

    pub fn get_range(&mut self, value: ValueId) -> crate::range::Range {
        self.range_assumptions.get_range(value)
    }

    /// Run the complete egraph pass
    pub fn run(&mut self) {
        // Single-pass aegraph construction: domtree walk with inline range
        // propagation, branch-condition learning, and conditional union creation.
        self.remove_pure_and_optimize();

        // Safety-net redundant phi elimination (handles cross-iteration
        // convergence the single inline pass cannot reach).
        if self.path_sensitive {
            self.eliminate_redundant_params();
        }

        // Apply GVN mapping to instruction args before elaboration so the
        // elaborator sees the GVN-canonical values.
        self.apply_gvn_to_args();

        // Extraction + elaboration (scoped, context-aware).
        // The elaborator walks the domtree, pushing/popping range assumption
        // scopes so that conditional unions created during
        // remove_pure_and_optimize are consulted in the correct branch context.
        {
            if self.path_sensitive {
                let mut elaborator = crate::elaborate::Elaborator::with_range_assumptions(
                    &mut self.dfg,
                    &self.domtree,
                    &mut self.stats,
                    &mut self.range_assumptions,
                    &self.block_entry_facts,
                    &self.cfg_preds,
                );
                elaborator.elaborate(&self.layout);
                elaborator.rewrite_args(&self.layout);
            } else {
                let mut elaborator = crate::elaborate::Elaborator::new(
                    &mut self.dfg,
                    &self.domtree,
                    &mut self.stats,
                );
                elaborator.elaborate(&self.layout);
                elaborator.rewrite_args(&self.layout);
            }
        }
        self.rebuild_layout();
        self.check_no_unions();

        // CFG cleanups (separate from aegraph construction)
        for _ in 0..4 {
            self.eliminate_dead_blocks();
            self.simplify_constant_brif();
            self.simplify_trivial_blocks();
            self.simplify_redundant_brif();
            self.simplify_uniform_returns();
        }
        self.eliminate_dead_blocks();

        // Print final statistics
        // self.stats.print_summary();
        // self.rewrite_engine.print_stats();
    }

    /// Resolve GVN mappings in all instruction arguments so that the
    /// elaborator (which is unaware of `gvn_mapping`) sees canonical values.
    fn apply_gvn_to_args(&mut self) {
        let all_insts: Vec<InstId> = self.dfg.insts.keys().copied().collect();
        for inst_id in all_insts {
            let mut inst = self.dfg.insts[&inst_id].clone();
            let mut changed = false;
            for arg in &mut inst.args {
                let resolved = self.fully_resolve(*arg);
                if resolved != *arg {
                    *arg = resolved;
                    changed = true;
                }
            }
            if changed {
                self.dfg.insts.insert(inst_id, inst);
            }
        }
    }

    /// Rebuild the layout to only include necessary instructions
    fn rebuild_layout(&mut self) {
        // For each block, rebuild its instruction list
        for &block_id in &self.layout.blocks.clone() {
            let block_data = self.layout.block_data.get(&block_id).unwrap();
            let mut needed_values = HashSet::new();
            let mut skeleton_insts = Vec::new();

            // First pass: collect skeleton instructions and the values they use
            for &inst_id in &block_data.insts {
                let inst = &self.dfg.insts[&inst_id];

                if !inst.opcode.is_pure() {
                    // This is a skeleton instruction - keep it
                    skeleton_insts.push(inst_id);

                    // Mark all its arguments as needed
                    for &arg in &inst.args {
                        needed_values.insert(arg);
                    }
                }
            }

            // Second pass: for each needed value, find the instruction that produces it
            // We collect all pure instructions needed and their dependencies
            let mut pure_insts_set = HashSet::new();
            let mut work_queue: Vec<ValueId> = needed_values.iter().copied().collect();
            let mut processed = HashSet::new();

            while let Some(value) = work_queue.pop() {
                if !processed.insert(value) {
                    continue;
                }

                // Find the instruction that defines this value
                if let ValueDef::Inst(inst_id) = self.dfg.value_def(value) {
                    let inst = &self.dfg.insts[&inst_id];

                    // Only add pure instructions (skeleton ones are already added)
                    if inst.opcode.is_pure() {
                        pure_insts_set.insert(inst_id);

                        // This instruction's arguments are also needed
                        for &arg in &inst.args {
                            if !processed.contains(&arg) {
                                work_queue.push(arg);
                            }
                        }
                    }
                }
            }

            // Third pass: topologically sort pure instructions
            // An instruction must come after all instructions that define its arguments
            let pure_insts_to_add = self.topological_sort_instructions(&pure_insts_set);

            // Combine pure and skeleton instructions
            // Pure instructions (in dependency order) first, then skeleton
            let mut new_insts = pure_insts_to_add;
            new_insts.extend(skeleton_insts);

            // Update the block
            let block = self.layout.block_data.get_mut(&block_id).unwrap();
            block.insts = new_insts;
        }
    }

    /// Verify that no `ValueDef::Union` nodes are reachable from any
    /// instruction argument after elaboration and layout rebuild.
    #[cfg(debug_assertions)]
    fn check_no_unions(&self) {
        for &block_id in &self.layout.blocks {
            let block_data = &self.layout.block_data[&block_id];
            for &inst_id in &block_data.insts {
                let inst = &self.dfg.insts[&inst_id];
                for &arg in &inst.args {
                    if let ValueDef::Union(_, _) = self.dfg.value_def(arg) {
                        panic!(
                            "Union node {:?} still reachable at inst {:?} in block {:?}",
                            arg, inst_id, block_id
                        );
                    }
                }
            }
            if let Some(term_id) = block_data.terminator {
                let inst = &self.dfg.insts[&term_id];
                for &arg in &inst.args {
                    if let ValueDef::Union(_, _) = self.dfg.value_def(arg) {
                        panic!(
                            "Union node {:?} still reachable at inst {:?} in block {:?}",
                            arg, term_id, block_id
                        );
                    }
                }
            }
        }
    }

    /// No-op in release builds; the debug version panics on residual unions.
    #[cfg(not(debug_assertions))]
    fn check_no_unions(&self) {}

    /// Topologically sort instructions so that definitions come before uses
    fn topological_sort_instructions(&self, inst_set: &HashSet<InstId>) -> Vec<InstId> {
        // Build a map from value to the instruction that defines it
        let mut value_to_inst: HashMap<ValueId, InstId> = HashMap::new();
        for &inst_id in inst_set {
            if let Some(result) = self.dfg.inst_results(inst_id).first() {
                value_to_inst.insert(*result, inst_id);
            }
        }

        // Build dependency graph: for each instruction, which instructions must come before it?
        let mut in_degree: HashMap<InstId, usize> = HashMap::new();
        let mut dependents: HashMap<InstId, Vec<InstId>> = HashMap::new();

        for &inst_id in inst_set {
            in_degree.entry(inst_id).or_insert(0);
            dependents.entry(inst_id).or_insert_with(Vec::new);

            let inst = &self.dfg.insts[&inst_id];
            for &arg in &inst.args {
                // If the argument is defined by an instruction in our set, add dependency
                if let Some(&defining_inst) = value_to_inst.get(&arg) {
                    if inst_set.contains(&defining_inst) && defining_inst != inst_id {
                        *in_degree.entry(inst_id).or_insert(0) += 1;
                        dependents
                            .entry(defining_inst)
                            .or_insert_with(Vec::new)
                            .push(inst_id);
                    }
                }
            }
        }

        // Kahn's algorithm for topological sort
        let mut result = Vec::new();
        let mut queue: Vec<InstId> = in_degree
            .iter()
            .filter(|(_, &degree)| degree == 0)
            .map(|(&inst_id, _)| inst_id)
            .collect();

        // Sort the initial queue by InstId for deterministic output
        queue.sort();

        while let Some(inst_id) = queue.pop() {
            result.push(inst_id);

            if let Some(deps) = dependents.get(&inst_id) {
                for &dependent in deps {
                    if let Some(degree) = in_degree.get_mut(&dependent) {
                        *degree -= 1;
                        if *degree == 0 {
                            queue.push(dependent);
                            queue.sort(); // Keep sorted for determinism
                        }
                    }
                }
            }
        }

        result
    }

    /// Add a custom rewrite rule to the engine
    pub fn add_rewrite_rule(&mut self, rule: Rewrite) {
        self.rewrite_engine.add_rule(rule);
    }

    /// Check whether `value` was remapped by GVN or redundant-phi elimination.
    /// Returns `true` if the value has a recorded canonical replacement.
    pub fn is_gvn_remapped(&self, value: ValueId) -> bool {
        self.gvn_mapping.contains_key(&value)
    }

    /// Expose the complete GVN mapping for testing purposes.
    pub fn gvn_mapping(&self) -> &HashMap<ValueId, ValueId> {
        &self.gvn_mapping
    }

    /// If every return instruction in the function returns the same value,
    /// and that value is available in the entry block or is a constant,
    /// replace the entire function body with `return value`.
    fn simplify_uniform_returns(&mut self) {
        let entry = match self.layout.entry_block() {
            Some(b) => b,
            None => return,
        };

        let mut return_values: Vec<ValueId> = Vec::new();
        for &block_id in &self.layout.blocks {
            if let Some(block) = self.layout.block_data.get(&block_id) {
                for &inst_id in &block.insts {
                    let inst = &self.dfg.insts[&inst_id];
                    if inst.opcode == Opcode::Return {
                        if let Some(&ret_val) = inst.args.first() {
                            return_values.push(ret_val);
                        }
                    }
                }
            }
        }

        if return_values.is_empty() {
            return;
        }

        let first = return_values[0];
        if !return_values.iter().all(|&v| v == first) {
            return;
        }

        let entry_data = self.layout.block_data.get(&entry).unwrap();
        let is_entry_param = entry_data.params.contains(&first);
        let is_entry_defined = entry_data
            .insts
            .iter()
            .any(|&iid| self.dfg.inst_results(iid).contains(&first));

        if !is_entry_param && !is_entry_defined {
            if let Some(c) = self.dfg.value_imm(first) {
                let const_inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, c);
                let const_inst_id = self.dfg.make_inst(const_inst);
                let const_val = self.dfg.make_inst_result(const_inst_id, Type::I32);

                let ret_inst = Instruction::new(Opcode::Return, vec![const_val], Type::I32);
                let ret_id = self.dfg.make_inst(ret_inst);

                let entry_data = self.layout.block_data.get_mut(&entry).unwrap();
                entry_data.insts = vec![const_inst_id, ret_id];
                self.layout.blocks = vec![entry];
                return;
            }
            return;
        }

        let entry_data = self.layout.block_data.get_mut(&entry).unwrap();
        if let Some(&last_inst_id) = entry_data.insts.last() {
            let last = &self.dfg.insts[&last_inst_id];
            if last.branch_info.is_some() {
                let ret_inst = Instruction::new(Opcode::Return, vec![first], Type::I32);
                let ret_id = self.dfg.make_inst(ret_inst);

                let mut needed = HashSet::new();
                let mut work = vec![first];
                while let Some(v) = work.pop() {
                    if let ValueDef::Inst(iid) = self.dfg.value_def(v) {
                        if needed.insert(iid) {
                            for &arg in &self.dfg.insts[&iid].args {
                                work.push(arg);
                            }
                        }
                    }
                }

                let old_insts = entry_data.insts.clone();
                entry_data.insts = old_insts
                    .into_iter()
                    .filter(|iid| needed.contains(iid))
                    .collect();
                entry_data.insts.push(ret_id);

                self.layout.blocks = vec![entry];
            }
        }
    }

    /// Simplify brif instructions whose condition is a known constant.
    fn simplify_constant_brif(&mut self) {
        let all_insts: Vec<InstId> = self.dfg.insts.keys().copied().collect();
        for inst_id in all_insts {
            let inst = self.dfg.insts[&inst_id].clone();
            if let Some(BranchInfo::Conditional(then_b, tc, else_b, ec)) = &inst.branch_info {
                let cond = inst.args[0];
                if let Some(val) = self.dfg.value_imm(cond) {
                    let (target, count, offset) = if val != 0 {
                        (*then_b, *tc, 1usize)
                    } else {
                        (*else_b, *ec, 1 + tc)
                    };
                    let new_args: Vec<ValueId> = inst.args[offset..offset + count].to_vec();
                    let new_inst = Instruction::with_branch(
                        Opcode::Branch,
                        new_args,
                        inst.ty,
                        BranchInfo::Jump(target),
                    );
                    self.dfg.insts.insert(inst_id, new_inst);
                }
            }
        }
    }

    /// Eliminate dead (unreachable) blocks.
    fn eliminate_dead_blocks(&mut self) {
        let entry = match self.layout.entry_block() {
            Some(b) => b,
            None => return,
        };

        let mut reachable = HashSet::new();
        let mut work = vec![entry];
        while let Some(b) = work.pop() {
            if !reachable.insert(b) {
                continue;
            }
            if let Some(block) = self.layout.block_data.get(&b) {
                for &inst_id in &block.insts {
                    let inst = &self.dfg.insts[&inst_id];
                    match &inst.branch_info {
                        Some(BranchInfo::Jump(t)) => work.push(*t),
                        Some(BranchInfo::Conditional(t, _, e, _)) => {
                            work.push(*t);
                            work.push(*e);
                        }
                        _ => {}
                    }
                }
            }
        }

        self.layout.blocks.retain(|b| reachable.contains(b));
    }

    /// Simplify trivial blocks (single-instruction blocks).
    fn simplify_trivial_blocks(&mut self) {
        let mut changed = true;
        while changed {
            changed = false;
            let blocks = self.layout.blocks.clone();
            for &block_id in &blocks {
                let block = match self.layout.block_data.get(&block_id) {
                    Some(b) => b.clone(),
                    None => continue,
                };

                if block.insts.len() != 1 {
                    continue;
                }
                let inst = &self.dfg.insts[&block.insts[0]];

                // Case 1: Block with only a return — inline into predecessors
                if inst.opcode == Opcode::Return && !inst.args.is_empty() {
                    let ret_val = inst.args[0];
                    if Some(block_id) != self.layout.entry_block() {
                        let param_idx = block.params.iter().position(|&p| p == ret_val);

                        let all_insts: Vec<InstId> = self.dfg.insts.keys().copied().collect();
                        for pred_inst_id in all_insts {
                            let pred_inst = self.dfg.insts[&pred_inst_id].clone();
                            match &pred_inst.branch_info {
                                Some(BranchInfo::Jump(t)) if *t == block_id => {
                                    let actual_val = if let Some(idx) = param_idx {
                                        pred_inst.args.get(idx).copied().unwrap_or(ret_val)
                                    } else {
                                        ret_val
                                    };
                                    let new_inst = Instruction::new(
                                        Opcode::Return,
                                        vec![actual_val],
                                        pred_inst.ty,
                                    );
                                    self.dfg.insts.insert(pred_inst_id, new_inst);
                                    changed = true;
                                }
                                Some(BranchInfo::Conditional(then_b, tc, else_b, _ec)) => {
                                    let then_b = *then_b;
                                    let tc = *tc;
                                    let else_b = *else_b;

                                    if then_b != block_id && else_b != block_id {
                                        continue;
                                    }

                                    if then_b == block_id && else_b == block_id {
                                        let then_val = if let Some(idx) = param_idx {
                                            pred_inst.args.get(1 + idx).copied().unwrap_or(ret_val)
                                        } else {
                                            ret_val
                                        };
                                        let else_val = if let Some(idx) = param_idx {
                                            pred_inst
                                                .args
                                                .get(1 + tc + idx)
                                                .copied()
                                                .unwrap_or(ret_val)
                                        } else {
                                            ret_val
                                        };
                                        if then_val == else_val {
                                            let new_inst = Instruction::new(
                                                Opcode::Return,
                                                vec![then_val],
                                                pred_inst.ty,
                                            );
                                            self.dfg.insts.insert(pred_inst_id, new_inst);
                                            changed = true;
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    continue;
                }

                // Case 2: Block with only a jump — forward predecessors
                let (target, jump_args) = match &inst.branch_info {
                    Some(BranchInfo::Jump(t)) => (*t, inst.args.clone()),
                    _ => continue,
                };

                if Some(block_id) == self.layout.entry_block() || target == block_id {
                    continue;
                }

                let all_insts: Vec<InstId> = self.dfg.insts.keys().copied().collect();
                for pred_inst_id in all_insts {
                    let pred_inst = self.dfg.insts[&pred_inst_id].clone();
                    match &pred_inst.branch_info {
                        Some(BranchInfo::Jump(t)) if *t == block_id => {
                            let mut new_args = Vec::new();
                            for &jump_arg in &jump_args {
                                let resolved = block
                                    .params
                                    .iter()
                                    .position(|&p| p == jump_arg)
                                    .and_then(|idx| pred_inst.args.get(idx).copied())
                                    .unwrap_or(jump_arg);
                                new_args.push(resolved);
                            }
                            let mut new_inst = pred_inst.clone();
                            new_inst.branch_info = Some(BranchInfo::Jump(target));
                            new_inst.args = new_args;
                            self.dfg.insts.insert(pred_inst_id, new_inst);
                            changed = true;
                        }
                        Some(BranchInfo::Conditional(then_b, tc, else_b, ec)) => {
                            let then_b = *then_b;
                            let tc = *tc;
                            let else_b = *else_b;
                            let ec = *ec;

                            if then_b != block_id && else_b != block_id {
                                continue;
                            }

                            let mut new_inst = pred_inst.clone();
                            if then_b == block_id {
                                let then_args: Vec<ValueId> =
                                    (0..tc).map(|i| pred_inst.args[1 + i]).collect();
                                let mut new_then_args = Vec::new();
                                for &jump_arg in &jump_args {
                                    let resolved = block
                                        .params
                                        .iter()
                                        .position(|&p| p == jump_arg)
                                        .and_then(|idx| then_args.get(idx).copied())
                                        .unwrap_or(jump_arg);
                                    new_then_args.push(resolved);
                                }
                                let else_args: Vec<ValueId> =
                                    (0..ec).map(|i| pred_inst.args[1 + tc + i]).collect();
                                let new_tc = new_then_args.len();
                                new_inst.args = vec![pred_inst.args[0]];
                                new_inst.args.extend(new_then_args);
                                new_inst.args.extend(&else_args);
                                new_inst.branch_info =
                                    Some(BranchInfo::Conditional(target, new_tc, else_b, ec));
                                changed = true;
                            }
                            if else_b == block_id {
                                let current_info = new_inst.branch_info.clone().unwrap();
                                let (cur_then, cur_tc, _, _) = match current_info {
                                    BranchInfo::Conditional(t, tc, _, ec) => (t, tc, block_id, ec),
                                    _ => unreachable!(),
                                };
                                let else_start = 1 + cur_tc;
                                let else_args: Vec<ValueId> = new_inst.args[else_start..].to_vec();
                                let mut new_else_args = Vec::new();
                                for &jump_arg in &jump_args {
                                    let resolved = block
                                        .params
                                        .iter()
                                        .position(|&p| p == jump_arg)
                                        .and_then(|idx| else_args.get(idx).copied())
                                        .unwrap_or(jump_arg);
                                    new_else_args.push(resolved);
                                }
                                let new_ec = new_else_args.len();
                                new_inst.args.truncate(else_start);
                                new_inst.args.extend(new_else_args);
                                new_inst.branch_info =
                                    Some(BranchInfo::Conditional(cur_then, cur_tc, target, new_ec));
                                changed = true;
                            }
                            self.dfg.insts.insert(pred_inst_id, new_inst);
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    /// Simplify redundant brif: if both branches go to the same block with the
    /// same arguments, replace with an unconditional jump.
    fn simplify_redundant_brif(&mut self) {
        let all_insts: Vec<InstId> = self.dfg.insts.keys().copied().collect();
        for inst_id in all_insts {
            let inst = self.dfg.insts[&inst_id].clone();
            if let Some(BranchInfo::Conditional(then_b, tc, else_b, ec)) = &inst.branch_info {
                if then_b == else_b && tc == ec {
                    let then_args: Vec<ValueId> = (0..*tc).map(|i| inst.args[1 + i]).collect();
                    let else_args: Vec<ValueId> = (0..*ec).map(|i| inst.args[1 + tc + i]).collect();
                    if then_args == else_args {
                        let new_inst = Instruction::with_branch(
                            Opcode::Branch,
                            then_args,
                            inst.ty,
                            BranchInfo::Jump(*then_b),
                        );
                        self.dfg.insts.insert(inst_id, new_inst);
                    }
                }
            }
        }
    }

    /// Redundant phi (block-param) elimination.
    ///
    /// Safety-net fallback: builds a fresh param→sources map from the layout,
    /// then eliminates any block param whose every predecessor passes the same
    /// canonical value. Iterates to a fixed point.
    fn eliminate_redundant_params(&mut self) {
        // Build a local param_sources map by scanning the layout.
        let mut param_sources: HashMap<ValueId, Vec<(BlockId, ValueId)>> = HashMap::new();
        for &block_id in &self.layout.blocks {
            let block = &self.layout.block_data[&block_id];
            for &inst_id in &block.insts {
                let inst = &self.dfg.insts[&inst_id];
                match &inst.branch_info {
                    Some(BranchInfo::Jump(target)) => {
                        if let Some(target_block) = self.layout.block_data.get(target) {
                            for (idx, &param) in target_block.params.iter().enumerate() {
                                if let Some(&incoming) = inst.args.get(idx) {
                                    param_sources
                                        .entry(param)
                                        .or_insert_with(Vec::new)
                                        .push((block_id, incoming));
                                }
                            }
                        }
                    }
                    Some(BranchInfo::Conditional(
                        then_block,
                        then_count,
                        else_block,
                        else_count,
                    )) => {
                        let then_block = *then_block;
                        let then_count = *then_count;
                        let else_block = *else_block;
                        let else_count = *else_count;
                        if let Some(tb) = self.layout.block_data.get(&then_block) {
                            for i in 0..then_count {
                                if let (Some(&param), Some(&incoming)) =
                                    (tb.params.get(i), inst.args.get(1 + i))
                                {
                                    param_sources
                                        .entry(param)
                                        .or_default()
                                        .push((block_id, incoming));
                                }
                            }
                        }
                        if let Some(eb) = self.layout.block_data.get(&else_block) {
                            for i in 0..else_count {
                                if let (Some(&param), Some(&incoming)) =
                                    (eb.params.get(i), inst.args.get(1 + then_count + i))
                                {
                                    param_sources
                                        .entry(param)
                                        .or_default()
                                        .push((block_id, incoming));
                                }
                            }
                        }
                    }
                    None => {}
                }
            }
        }

        let mut changed = true;
        while changed {
            changed = false;

            let params: Vec<ValueId> = param_sources.keys().copied().collect();

            for param in params {
                if self
                    .gvn_mapping
                    .get(&param)
                    .map(|&v| v != param)
                    .unwrap_or(false)
                {
                    continue;
                }
                match self.dfg.value_defs.get(&param) {
                    Some(ValueDef::BlockParam(_, _)) => {}
                    _ => continue,
                }

                let sources = match param_sources.get(&param) {
                    Some(s) if !s.is_empty() => s.clone(),
                    _ => continue,
                };

                let mut non_self_canonicals: Vec<ValueId> = Vec::new();
                let mut all_self = true;

                for &(_, incoming) in &sources {
                    let resolved = self.fully_resolve(incoming);
                    if resolved == param {
                        // Loop-carried self-reference
                    } else {
                        all_self = false;
                        non_self_canonicals.push(resolved);
                    }
                }

                if all_self || non_self_canonicals.is_empty() {
                    continue;
                }

                let first = non_self_canonicals[0];
                if non_self_canonicals.iter().all(|&v| v == first) {
                    let replacement_def = match self.dfg.value_defs.get(&first).copied() {
                        Some(d) => d,
                        None => continue,
                    };
                    self.dfg.value_defs.insert(param, replacement_def);
                    self.gvn_mapping.insert(param, first);
                    changed = true;
                }
            }
        }
    }

    /// Follow `gvn_mapping` transitively until no further mapping exists.
    ///
    /// This is used inside `eliminate_redundant_params` to make replacements
    /// made in earlier iterations visible when computing canonical values for
    /// later ones (e.g., v2 → v0 in iteration 1 allows v8 → v0 in iteration 2).
    /// The depth limit guards against unexpected cycles.
    pub fn fully_resolve(&self, value: ValueId) -> ValueId {
        let mut current = value;
        for _ in 0..64 {
            match self.gvn_mapping.get(&current).copied() {
                Some(next) if next != current => current = next,
                _ => return current,
            }
        }
        current // cycle-safe fallback
    }

    /// Remove pure operations from layout and build egraph
    ///
    /// Single domtree-preorder walk that:
    /// - Builds block-param incoming maps inline (replacing build_param_sources)
    /// - Records branch conditions inline (replacing the pre-scan)
    /// - Propagates ranges, creates unions, and applies rewrites
    fn remove_pure_and_optimize(&mut self) {
        let mut value_to_opt_value: HashMap<ValueId, ValueId> = HashMap::new();

        let mut gvn_map: ScopedHashMap<(Type, Instruction), Option<ValueId>> =
            ScopedHashMap::with_capacity(100);

        let mut available_block: HashMap<ValueId, BlockId> = HashMap::new();

        let mut gvn_map_blocks: Vec<BlockId> = Vec::new();

        // Pre-scan: build (target_block, param_index) → (predecessor, incoming_value)
        // from the layout. Values are original (unrewritten); resolved through
        // value_to_opt_value at read time. This must be pre-computed because
        // CFG predecessors are not guaranteed to precede successors in a
        // domtree-preorder walk (merge blocks may be visited before siblings).
        let mut block_param_incoming: HashMap<(BlockId, usize), Vec<(BlockId, ValueId)>> =
            HashMap::new();
        for &blk in &self.layout.blocks {
            let bd = &self.layout.block_data[&blk];
            for &iid in &bd.insts {
                let inst = &self.dfg.insts[&iid];
                match &inst.branch_info {
                    Some(BranchInfo::Jump(target)) => {
                        for (idx, &arg) in inst.args.iter().enumerate() {
                            block_param_incoming
                                .entry((*target, idx))
                                .or_default()
                                .push((blk, arg));
                        }
                    }
                    Some(BranchInfo::Conditional(then_b, tc, else_b, ec)) => {
                        let tc = *tc;
                        let ec = *ec;
                        for idx in 0..tc {
                            if let Some(&arg) = inst.args.get(1 + idx) {
                                block_param_incoming
                                    .entry((*then_b, idx))
                                    .or_default()
                                    .push((blk, arg));
                            }
                        }
                        for idx in 0..ec {
                            if let Some(&arg) = inst.args.get(1 + tc + idx) {
                                block_param_incoming
                                    .entry((*else_b, idx))
                                    .or_default()
                                    .push((blk, arg));
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        // Build CFG predecessors map for sound range-fact joining at merge points.
        let mut cfg_preds: HashMap<BlockId, Vec<BlockId>> = HashMap::new();
        for &blk in &self.layout.blocks {
            cfg_preds.entry(blk).or_default();
            let bd = &self.layout.block_data[&blk];
            for &iid in &bd.insts {
                let inst = &self.dfg.insts[&iid];
                match &inst.branch_info {
                    Some(BranchInfo::Jump(target)) => {
                        cfg_preds.entry(*target).or_default().push(blk);
                    }
                    Some(BranchInfo::Conditional(then_b, _, else_b, _)) => {
                        cfg_preds.entry(*then_b).or_default().push(blk);
                        cfg_preds.entry(*else_b).or_default().push(blk);
                    }
                    _ => {}
                }
            }
        }

        // Range facts per edge (pred, target) and edge conditions populated inline during the walk
        let mut block_entry_facts: HashMap<(BlockId, BlockId), Vec<(ValueId, Range)>> =
            HashMap::new();
        let mut block_edge_conditions: HashMap<
            (BlockId, BlockId),
            Vec<(ValueId, Option<i64>, Opcode, bool)>,
        > = HashMap::new();

        let root = self.layout.entry_block().unwrap();

        enum StackEntry {
            Visit(BlockId),
            Pop,
        }

        let mut block_stack = vec![StackEntry::Visit(root)];

        while let Some(entry) = block_stack.pop() {
            match entry {
                StackEntry::Visit(block) => {
                    block_stack.push(StackEntry::Pop);
                    // Push children in reverse order so that lower block IDs
                    // (typically predecessors / branch targets) are visited
                    // first. This ensures merge blocks see their predecessors'
                    // value_to_opt_value mappings.
                    for &child in self.domtree.children(block).iter().rev() {
                        block_stack.push(StackEntry::Visit(child));
                    }

                    gvn_map.increment_depth();
                    gvn_map_blocks.push(block);
                    self.range_assumptions.push_scope();

                    let block_data = self.layout.block_data[&block].clone();

                    // 1. Apply range facts from incoming edges (populated by
                    //    predecessors that were already visited in domtree order).
                    //    For merge blocks with multiple predecessors, we compute the
                    //    UNION (join) of ranges per value across all edges to avoid
                    //    unsoundly intersecting contradictory facts.
                    if self.path_sensitive {
                        let preds = cfg_preds.get(&block).cloned().unwrap_or_default();
                        let num_edges = preds.len();
                        // For each edge, compute the per-value intersected range
                        // (multiple facts about the same value on one edge are
                        // intersected), then union those across edges.
                        let mut value_edge_count: HashMap<ValueId, usize> = HashMap::new();
                        let mut value_union: HashMap<ValueId, Range> = HashMap::new();
                        for pred in &preds {
                            if let Some(facts) = block_entry_facts.get(&(*pred, block)) {
                                // Intersect all facts for the same value on this edge
                                let mut per_edge: HashMap<ValueId, Range> = HashMap::new();
                                for &(value, range) in facts {
                                    per_edge
                                        .entry(value)
                                        .and_modify(|acc| {
                                            *acc = match acc.intersect(&range) {
                                                Some(r) => r,
                                                None => Range::empty(),
                                            };
                                        })
                                        .or_insert(range);
                                }
                                // Union across edges
                                for (value, range) in per_edge {
                                    *value_edge_count.entry(value).or_insert(0) += 1;
                                    value_union
                                        .entry(value)
                                        .and_modify(|acc| {
                                            *acc = acc.union(&range);
                                        })
                                        .or_insert(range);
                                }
                            }
                        }
                        // Only apply facts that appear on ALL incoming edges
                        for (value, range) in &value_union {
                            if value_edge_count.get(value) == Some(&num_edges)
                                && !range.is_unbounded()
                            {
                                self.range_assumptions.assume_range(*value, *range);
                            }
                        }
                    }

                    // Apply nonzero marks from incoming edge conditions
                    // Only mark nonzero if ALL predecessor edges agree.
                    if self.path_sensitive {
                        let preds = cfg_preds.get(&block).cloned().unwrap_or_default();
                        let num_preds = preds.len();
                        if num_preds > 0 {
                            let mut nonzero_edge_count: HashMap<ValueId, usize> = HashMap::new();
                            for pred in &preds {
                                if let Some(conditions) = block_edge_conditions.get(&(*pred, block))
                                {
                                    for &(lhs, rhs_const, opcode, is_true_branch) in conditions {
                                        if opcode == Opcode::Ne
                                            && is_true_branch
                                            && rhs_const == Some(0)
                                        {
                                            *nonzero_edge_count.entry(lhs).or_insert(0) += 1;
                                        }
                                    }
                                }
                            }
                            for (value, count) in &nonzero_edge_count {
                                if *count == num_preds {
                                    self.range_assumptions.mark_nonzero(*value);
                                }
                            }
                        }
                    }

                    // 2. Process block params: identity-map, per-edge range join,
                    //    singleton substitution, then see-through + conditional unions
                    for (param_idx, &param) in block_data.params.iter().enumerate() {
                        value_to_opt_value.insert(param, param);
                        available_block.insert(param, block);

                        // Per-edge join-point range propagation
                        if self.path_sensitive {
                            let key = (block, param_idx);
                            if let Some(incoming) = block_param_incoming.get(&key) {
                                let mut joined = Range::empty();
                                for &(pred, src_val) in incoming {
                                    if pred == block {
                                        continue;
                                    }
                                    let mut src_range = self.range_assumptions.get_range(src_val);
                                    // Narrow by edge-specific conditions
                                    if let Some(conditions) =
                                        block_edge_conditions.get(&(pred, block))
                                    {
                                        for &(lhs, rhs_const, opcode, is_true_branch) in conditions
                                        {
                                            let facts = learn_from_comparison(
                                                opcode,
                                                lhs,
                                                rhs_const,
                                                is_true_branch,
                                            );
                                            for (value, range) in facts {
                                                if value == src_val {
                                                    src_range = match src_range.intersect(&range) {
                                                        Some(r) => r,
                                                        None => Range::empty(),
                                                    };
                                                }
                                            }
                                        }
                                    }
                                    joined = joined.union(&src_range);
                                }
                                if !joined.is_empty() && !joined.is_unbounded() {
                                    self.range_assumptions.assume_range(param, joined);
                                }
                            }

                            // Singleton-range constant substitution
                            let range = self.range_assumptions.get_range(param);
                            if let Some(c) = range.as_singleton() {
                                let const_inst =
                                    Instruction::with_imm(Opcode::Const, vec![], Type::I32, c);
                                let key = (Type::I32, const_inst.clone());
                                let const_val = if let Some(Some(existing)) = gvn_map.get(&key) {
                                    *existing
                                } else {
                                    let const_inst_id = self.dfg.make_inst(const_inst);
                                    let cv = self.dfg.make_inst_result(const_inst_id, Type::I32);
                                    gvn_map.insert(key, Some(cv));
                                    available_block.insert(cv, block);
                                    cv
                                };
                                value_to_opt_value.insert(param, const_val);
                            }
                        }
                    }

                    if self.path_sensitive {
                        // Inline block-param equivalence propagation
                        for (param_idx, &param) in block_data.params.iter().enumerate() {
                            let incoming_key = (block, param_idx);
                            let incoming = match block_param_incoming.get(&incoming_key) {
                                Some(s) if !s.is_empty() => s.clone(),
                                _ => continue,
                            };

                            // Resolve each incoming value through value_to_opt_value
                            let resolved: Vec<(BlockId, ValueId)> = incoming
                                .iter()
                                .map(|&(pred, src)| {
                                    let resolved =
                                        value_to_opt_value.get(&src).copied().unwrap_or(src);
                                    (pred, resolved)
                                })
                                .collect();

                            let non_self: Vec<(BlockId, ValueId)> = resolved
                                .iter()
                                .filter(|&&(_, v)| v != param)
                                .copied()
                                .collect();

                            if non_self.is_empty() {
                                continue;
                            }

                            let first_val = non_self[0].1;
                            let mut all_agree = non_self.iter().all(|&(_, v)| v == first_val);

                            if !all_agree {
                                let first_const = self.dfg.value_imm(first_val);
                                if let Some(c) = first_const {
                                    all_agree = non_self
                                        .iter()
                                        .all(|&(_, v)| self.dfg.value_imm(v) == Some(c));
                                }
                            }

                            if all_agree && first_val != param {
                                let union_node = self.dfg.make_union(param, first_val);
                                let current =
                                    value_to_opt_value.get(&param).copied().unwrap_or(param);
                                if current == param {
                                    value_to_opt_value.insert(param, first_val);
                                    if let Some(&avail) = available_block.get(&first_val) {
                                        available_block.insert(param, avail);
                                    }
                                }
                                available_block.insert(union_node, block);
                                self.stats.union += 1;
                            } else if !all_agree && self.path_sensitive {
                                for &(pred, resolved_incoming) in &resolved {
                                    if resolved_incoming == param {
                                        continue;
                                    }
                                    let assumptions = build_edge_assumptions_inline(
                                        pred,
                                        block,
                                        &block_edge_conditions,
                                    );
                                    self.dfg.make_conditional_union(
                                        param,
                                        resolved_incoming,
                                        assumptions,
                                    );
                                }
                            }
                        }
                    }

                    // Process instructions
                    let inst_ids: Vec<_> = block_data.insts.clone();

                    for inst_id in inst_ids {
                        let inst = self.dfg.insts[&inst_id].clone();

                        let mut rewritten_inst = inst.clone();
                        for arg in &mut rewritten_inst.args {
                            if let Some(&opt_value) = value_to_opt_value.get(arg) {
                                *arg = opt_value;
                            }
                        }
                        self.dfg.insts.insert(inst_id, rewritten_inst.clone());

                        let mut ctx = OptimizeCtx {
                            dfg: &mut self.dfg,
                            value_to_opt_value: &mut value_to_opt_value,
                            gvn_map: &mut gvn_map,
                            gvn_map_blocks: &gvn_map_blocks,
                            available_block: &mut available_block,
                            stats: &mut self.stats,
                            domtree: &self.domtree,
                            rewrite_depth: 0,
                            subsume_values: HashSet::new(),
                            rewrite_engine: &mut self.rewrite_engine,
                            range_assumptions: &mut self.range_assumptions,
                            path_sensitive: self.path_sensitive,
                        };

                        if rewritten_inst.opcode.is_pure() {
                            ctx.insert_pure_enode(inst_id);
                        } else {
                            ctx.optimize_skeleton_inst(inst_id, block);
                        }

                        // Propagate ranges forward
                        if self.path_sensitive {
                            let inst_after = self.dfg.insts[&inst_id].clone();
                            let arg_ranges: Vec<crate::range::Range> = inst_after
                                .args
                                .iter()
                                .map(|&arg| self.range_assumptions.get_range(arg))
                                .collect();
                            let output_range = crate::range::compute_inst_range(
                                inst_after.opcode,
                                &arg_ranges,
                                inst_after.immediate,
                            );
                            if !output_range.is_unbounded() {
                                if let Some(&result) = self.dfg.inst_results(inst_id).first() {
                                    let opt_result =
                                        value_to_opt_value.get(&result).copied().unwrap_or(result);
                                    self.range_assumptions.assume_range(result, output_range);
                                    self.stats.ranges_propagated += 1;
                                    if opt_result != result {
                                        self.range_assumptions
                                            .assume_range(opt_result, output_range);
                                    }
                                    if let Some(c) = output_range.as_singleton() {
                                        let current = value_to_opt_value
                                            .get(&result)
                                            .copied()
                                            .unwrap_or(result);
                                        if current == result || current == opt_result {
                                            let const_inst = Instruction::with_imm(
                                                Opcode::Const,
                                                vec![],
                                                Type::I32,
                                                c,
                                            );
                                            let key = (Type::I32, const_inst.clone());
                                            let const_val = if let Some(Some(existing)) =
                                                gvn_map.get(&key)
                                            {
                                                *existing
                                            } else {
                                                let const_inst_id = self.dfg.make_inst(const_inst);
                                                let cv = self
                                                    .dfg
                                                    .make_inst_result(const_inst_id, Type::I32);
                                                gvn_map.insert(key, Some(cv));
                                                available_block.insert(cv, block);
                                                cv
                                            };
                                            let _ = self.dfg.make_union(opt_result, const_val);
                                            value_to_opt_value.insert(result, const_val);
                                            self.stats.union += 1;
                                        }
                                    }
                                }
                            }
                        }

                        // Inline branch-condition recording (replacing pre-scan).
                        // Recorded after arg rewriting so conditions reference
                        // optimized values. Branch conditions from dominators
                        // are always available for dominated blocks.
                        let inst_after = self.dfg.insts[&inst_id].clone();
                        if self.path_sensitive && inst_after.opcode == Opcode::CondBranch {
                            if let Some(BranchInfo::Conditional(then_b, _, else_b, _)) =
                                &inst_after.branch_info
                            {
                                let then_b = *then_b;
                                let else_b = *else_b;
                                if !inst_after.args.is_empty() {
                                    let cond_value = inst_after.args[0];
                                    // True branch: condition is nonzero.
                                    // Walk backward through band/bor/cmp to
                                    // learn all leaf-level range facts.
                                    {
                                        self.range_assumptions.push_scope();
                                        let then_facts = self
                                            .range_assumptions
                                            .refine_nonzero(cond_value, &self.dfg);
                                        self.range_assumptions.pop_scope();

                                        block_entry_facts
                                            .entry((block, then_b))
                                            .or_default()
                                            .extend(then_facts.iter().cloned());
                                        // Record leaf facts as edge conditions
                                        // so block-param range join can narrow
                                        // per-edge ranges.
                                        for &(value, range) in &then_facts {
                                            // Approximate: store as a >= min
                                            // and < max+1 pair. The edge
                                            // conditions use (lhs, rhs_const,
                                            // opcode, is_true_branch) tuples.
                                            // We record Sge for the lower bound.
                                            block_edge_conditions
                                                .entry((block, then_b))
                                                .or_default()
                                                .push((value, Some(range.min), Opcode::Sge, true));
                                            if range.max < i64::MAX {
                                                block_edge_conditions
                                                    .entry((block, then_b))
                                                    .or_default()
                                                    .push((
                                                        value,
                                                        Some(range.max + 1),
                                                        Opcode::Slt,
                                                        true,
                                                    ));
                                            }
                                        }
                                        if then_facts.is_empty() {
                                            // No leaf facts found — record
                                            // top-level cond != 0 as fallback.
                                            let ne_facts = learn_from_comparison(
                                                Opcode::Ne,
                                                cond_value,
                                                Some(0),
                                                true,
                                            );
                                            block_entry_facts
                                                .entry((block, then_b))
                                                .or_default()
                                                .extend(ne_facts);
                                            block_edge_conditions
                                                .entry((block, then_b))
                                                .or_default()
                                                .push((cond_value, Some(0), Opcode::Ne, true));
                                        }
                                    }

                                    // Else branch: condition is zero.
                                    {
                                        self.range_assumptions.push_scope();
                                        let else_facts = self
                                            .range_assumptions
                                            .refine_zero(cond_value, &self.dfg);
                                        self.range_assumptions.pop_scope();

                                        block_entry_facts
                                            .entry((block, else_b))
                                            .or_default()
                                            .extend(else_facts.iter().cloned());
                                        for &(value, range) in &else_facts {
                                            block_edge_conditions
                                                .entry((block, else_b))
                                                .or_default()
                                                .push((value, Some(range.min), Opcode::Sge, true));
                                            if range.max < i64::MAX {
                                                block_edge_conditions
                                                    .entry((block, else_b))
                                                    .or_default()
                                                    .push((
                                                        value,
                                                        Some(range.max + 1),
                                                        Opcode::Slt,
                                                        true,
                                                    ));
                                            }
                                        }
                                        if else_facts.is_empty() {
                                            let ne_facts = learn_from_comparison(
                                                Opcode::Ne,
                                                cond_value,
                                                Some(0),
                                                false,
                                            );
                                            block_entry_facts
                                                .entry((block, else_b))
                                                .or_default()
                                                .extend(ne_facts);
                                            block_edge_conditions
                                                .entry((block, else_b))
                                                .or_default()
                                                .push((cond_value, Some(0), Opcode::Ne, false));
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // Second pass: re-evaluate branch-terminator condition
                    // values that were GVN-mapped from a dominator block.
                    // Range-conditioned rewrites for such values were
                    // attempted in their defining block, where the current
                    // block's range assumptions were not yet in scope.
                    // Re-running apply_conditional_rewrites_and_store on them
                    // here lets range-based folds fire under the tightened
                    // scope (e.g., icmp.sge x, 0 → 1 once x is known to be
                    // non-negative on this branch). Restricted to brif
                    // condition values to keep the cost bounded.
                    if self.path_sensitive {
                        let mut cond_values: Vec<ValueId> = Vec::new();
                        for &iid in &block_data.insts {
                            let inst = &self.dfg.insts[&iid];
                            if inst.opcode == Opcode::CondBranch {
                                if let Some(&cond) = inst.args.first() {
                                    cond_values.push(cond);
                                }
                            }
                        }
                        for cond in cond_values {
                            let canonical = value_to_opt_value.get(&cond).copied().unwrap_or(cond);
                            let defined_in_dominator = available_block
                                .get(&canonical)
                                .copied()
                                .map(|b| b != block)
                                .unwrap_or(false);
                            if !defined_in_dominator {
                                continue;
                            }
                            let mut ctx = OptimizeCtx {
                                dfg: &mut self.dfg,
                                value_to_opt_value: &mut value_to_opt_value,
                                gvn_map: &mut gvn_map,
                                gvn_map_blocks: &gvn_map_blocks,
                                available_block: &mut available_block,
                                stats: &mut self.stats,
                                domtree: &self.domtree,
                                rewrite_depth: 0,
                                subsume_values: HashSet::new(),
                                rewrite_engine: &mut self.rewrite_engine,
                                range_assumptions: &mut self.range_assumptions,
                                path_sensitive: self.path_sensitive,
                            };
                            ctx.apply_conditional_rewrites_and_store(canonical);
                        }
                    }
                }

                StackEntry::Pop => {
                    gvn_map.decrement_depth();
                    gvn_map_blocks.pop();
                    self.range_assumptions.pop_scope();
                }
            }
        }
        // Save the GVN mapping for use in extraction
        self.gvn_mapping = value_to_opt_value;
        // Save block_entry_facts and cfg_preds for the elaborator's domtree walk
        self.block_entry_facts = block_entry_facts;
        self.cfg_preds = cfg_preds;
    }
}

/// Build an `AssumptionSet` capturing the range facts that hold on the edge
/// from `pred` to `target`, reading from the local `block_edge_conditions` map.
fn build_edge_assumptions_inline(
    pred: BlockId,
    target: BlockId,
    edge_conditions: &HashMap<(BlockId, BlockId), Vec<(ValueId, Option<i64>, Opcode, bool)>>,
) -> AssumptionSet {
    let mut assumptions = Vec::new();
    if let Some(conditions) = edge_conditions.get(&(pred, target)) {
        for &(lhs, rhs_const, opcode, is_true_branch) in conditions {
            let facts = learn_from_comparison(opcode, lhs, rhs_const, is_true_branch);
            for (value, range) in facts {
                assumptions.push((value, range));
            }
        }
    }
    AssumptionSet { assumptions }
}

/// Context for optimization
struct OptimizeCtx<'a> {
    dfg: &'a mut DataFlowGraph,
    value_to_opt_value: &'a mut HashMap<ValueId, ValueId>,
    gvn_map: &'a mut ScopedHashMap<(Type, Instruction), Option<ValueId>>,
    gvn_map_blocks: &'a Vec<BlockId>,
    available_block: &'a mut HashMap<ValueId, BlockId>,
    stats: &'a mut Stats,
    domtree: &'a DominatorTree,
    rewrite_depth: usize,
    subsume_values: HashSet<ValueId>,
    rewrite_engine: &'a mut RewriteEngine,
    range_assumptions: &'a mut RangeAssumptions,
    path_sensitive: bool,
}

impl<'a> OptimizeCtx<'a> {
    /// Insert a pure enode (instruction) into the egraph
    ///
    /// This is the core of the egraph construction:
    /// 1. Check GVN map - if instruction already exists, reuse it
    /// 2. If new, insert it and apply optimization rules
    /// 3. Create unions for equivalent forms
    fn insert_pure_enode(&mut self, inst_id: InstId) -> ValueId {
        self.stats.pure_inst += 1;

        let inst = self.dfg.insts[&inst_id].clone();
        let key = (inst.ty, inst.clone());

        let result = self.dfg.first_result(inst_id);
        let avail_block = self.get_available_block(inst_id);

        match self.gvn_map.entry(key) {
            ScopedEntry::Occupied(entry) => {
                // Found existing instruction
                self.stats.pure_inst_deduped += 1;

                if let Some(existing_value) = entry.get() {
                    self.value_to_opt_value.insert(result, *existing_value);

                    if let Some(&avail_block) = self.available_block.get(existing_value) {
                        self.available_block.insert(result, avail_block);
                    }

                    return *existing_value;
                }

                // Shouldn't reach here, but handle gracefully
                self.value_to_opt_value.insert(result, result);
                result
            }

            ScopedEntry::Vacant(entry) => {
                // New instruction - insert it
                self.stats.pure_inst_insert_new += 1;

                self.value_to_opt_value.insert(result, result);
                self.available_block.insert(result, avail_block);

                entry.insert(Some(result));

                // Apply rewrite rules and create unions
                let opt_result = self.apply_rewrites_and_union(result);

                // Second pass: create conditional unions for range-dependent rewrites
                if self.path_sensitive {
                    self.apply_conditional_rewrites_and_store(result);
                }

                opt_result
            }
        }
    }

    /// Apply rewrite rules and create union nodes
    ///
    /// This is the key integration point with the pattern-based rewrite system.
    fn apply_rewrites_and_union(&mut self, value: ValueId) -> ValueId {
        if self.rewrite_depth >= REWRITE_LIMIT {
            self.stats.rewrite_depth_limit += 1;
            return value;
        }

        self.rewrite_depth += 1;
        self.stats.rewrite_rule_invoked += 1;

        let mut union_value = value;

        // Range-conditioned rewrites are handled by
        // apply_conditional_rewrites_and_store, which decides whether to
        // create an unconditional or conditional union based on whether
        // the range facts are globally provable or branch-local.
        // Passing range assumptions here would create permanent
        // ValueDef::Union nodes from branch-local facts, which is unsound
        // because the extractor follows unions without range guards.
        let rewrites = self
            .rewrite_engine
            .apply_rewrites_with_ranges(self.dfg, value, None);

        for rewritten in rewrites {
            self.stats.rewrite_rule_results += 1;

            // Recursively optimize the rewritten value
            let opt_value = if let ValueDef::Inst(rewritten_inst) = self.dfg.value_def(rewritten) {
                // Check if we should process this rewrite
                if self.subsume_values.contains(&rewritten) {
                    self.stats.pure_inst_subsume += 1;
                    rewritten
                } else {
                    self.subsume_values.insert(rewritten);
                    self.insert_pure_enode(rewritten_inst)
                }
            } else {
                rewritten
            };

            // If the rewritten value is simpler (constant or block param), update
            // value_to_opt_value so that subsequent instruction arg rewrites see the
            // simplified form rather than the union node.
            if opt_value != value {
                let is_simpler = match self.dfg.value_defs.get(&opt_value).copied() {
                    Some(ValueDef::Inst(iid)) => self.dfg.insts[&iid].opcode == Opcode::Const,
                    Some(ValueDef::BlockParam(_, _)) => true,
                    _ => false,
                };
                if is_simpler {
                    self.value_to_opt_value.insert(value, opt_value);
                }
            }

            // Create union between original and rewritten
            let new_union = self.dfg.make_union(union_value, opt_value);
            self.stats.union += 1;

            // Merge availability: use the block that dominates both
            let avail = self.merge_availability(union_value, opt_value);
            self.available_block.insert(new_union, avail);

            union_value = new_union;
        }

        self.rewrite_depth -= 1;
        union_value
    }

    /// Returns `true` if the tightest known range for `value` in the current
    /// scope is strictly tighter than it would be in the root scope (depth 0).
    ///
    /// This distinguishes branch-local assumptions (pushed by dominator-tree
    /// traversal through conditional branches) from globally provable facts.
    fn is_branch_local(&self, value: ValueId, range: &Range) -> bool {
        if self.range_assumptions.depth() == 0 {
            return false;
        }
        let root = self.range_assumptions.root_range(value);
        // The current range is branch-local if it is strictly contained in
        // the root range (i.e., the root range alone does not prove this fact).
        !(root.min >= range.min && root.max <= range.max)
    }

    /// Handle ALL range-conditioned rewrites.  For each rewrite rule whose
    /// range-based conditions are currently satisfied, this method creates:
    ///
    /// - An **unconditional** union (via `make_union`) when every range fact
    ///   is globally provable (root scope suffices).
    /// - A **conditional** union (via `make_conditional_union`) when at least
    ///   one range fact is branch-local.
    fn apply_conditional_rewrites_and_store(&mut self, value: ValueId) {
        // Only consider instruction-defined values.
        let _ = match self.dfg.value_def(value) {
            ValueDef::Inst(id) => id,
            _ => return,
        };

        let ra_ptr = &*self.range_assumptions as *const RangeAssumptions;
        // SAFETY: The pointee is not mutated through the raw pointer while
        // this shared reference is live.  We only read range assumptions
        // below; `self.dfg` is the only field mutated during the loop.
        let ra = unsafe { &*ra_ptr };

        // Iterate over all rewrite rules to find ones with range-based conditions.
        for rule in &self.rewrite_engine.library.rules.clone() {
            // Skip rules without range-based conditions.
            let range_conditions = Self::collect_range_conditions(&rule.conditions);
            if range_conditions.is_empty() {
                continue;
            }

            // Try to match the LHS pattern.
            let matcher = PatternMatcher::new(self.dfg);
            let mut bindings = match matcher.match_pattern(&rule.lhs, value) {
                Some(b) => b,
                None => continue,
            };
            bindings.set_range_assumptions(ra);

            // Check that all conditions hold.
            if !rule.check_conditions(&bindings) {
                continue;
            }

            // Determine which range facts are branch-local.
            let mut branch_local_facts: Vec<(ValueId, Range)> = Vec::new();
            let mut all_global = true;

            for cond in &range_conditions {
                if let Some((var, required_range)) = Self::condition_to_range_fact(cond, &bindings)
                {
                    if self.is_branch_local(var, &required_range) {
                        branch_local_facts.push((var, required_range));
                        all_global = false;
                    }
                }
            }

            // Apply the RHS to produce the rewritten value.
            let mut applier = PatternApplier::new(self.dfg);
            let new_value = match applier.apply_pattern(&rule.rhs, &bindings) {
                Some(v) if v != value => v,
                _ => continue,
            };

            if all_global {
                // All range facts are globally provable — safe to create an
                // unconditional union.
                let union_node = self.dfg.make_union(value, new_value);
                self.stats.union += 1;

                // Propagate simpler-value information so the extractor sees it.
                let is_simpler = match self.dfg.value_defs.get(&new_value).copied() {
                    Some(ValueDef::Inst(iid)) => self.dfg.insts[&iid].opcode == Opcode::Const,
                    Some(ValueDef::BlockParam(_, _)) => true,
                    _ => false,
                };
                if is_simpler {
                    self.value_to_opt_value.insert(value, new_value);
                }

                // Ensure new_value has an availability entry before merging.
                if !self.available_block.contains_key(&new_value) {
                    if let Some(&blk) = self.available_block.get(&value) {
                        self.available_block.insert(new_value, blk);
                    }
                }
                let avail = self.merge_availability(value, new_value);
                self.available_block.insert(union_node, avail);
                continue;
            }

            // Some facts are branch-local — create a conditional union guarded
            // by the branch-local assumptions.
            let assumption_set = AssumptionSet {
                assumptions: branch_local_facts,
            };

            let _ = self
                .dfg
                .make_conditional_union(value, new_value, assumption_set);
        }
    }

    /// Collect range-based conditions from a slice of conditions (flattening And).
    fn collect_range_conditions(conditions: &[Condition]) -> Vec<Condition> {
        let mut result = Vec::new();
        for cond in conditions {
            match cond {
                Condition::InRange(..)
                | Condition::NonNegative(..)
                | Condition::Positive(..)
                | Condition::Negative(..)
                | Condition::RangeLessThan(..)
                | Condition::RangeGreaterThan(..)
                | Condition::RangeEquals(..)
                | Condition::Unreachable(..)
                | Condition::NonZero(..)
                | Condition::Custom { .. } => {
                    result.push(cond.clone());
                }
                Condition::And(inner) => {
                    result.extend(Self::collect_range_conditions(inner));
                }
                _ => {}
            }
        }
        result
    }

    /// Extract the `(ValueId, required Range)` from a range-based condition,
    /// using the bindings to resolve the variable to a concrete ValueId.
    fn condition_to_range_fact(cond: &Condition, bindings: &Bindings) -> Option<(ValueId, Range)> {
        match cond {
            Condition::InRange(var, min, max) => {
                let v = bindings.get_value(var)?;
                Some((v, Range::new(*min, *max)))
            }
            Condition::NonNegative(var) => {
                let v = bindings.get_value(var)?;
                Some((v, Range::non_negative()))
            }
            Condition::Positive(var) => {
                let v = bindings.get_value(var)?;
                Some((v, Range::positive()))
            }
            Condition::Negative(var) => {
                let v = bindings.get_value(var)?;
                Some((v, Range::negative()))
            }
            Condition::RangeLessThan(var, val) => {
                let v = bindings.get_value(var)?;
                Some((v, Range::new(i64::MIN, *val - 1)))
            }
            Condition::RangeGreaterThan(var, val) => {
                let v = bindings.get_value(var)?;
                Some((v, Range::new(*val + 1, i64::MAX)))
            }
            Condition::RangeEquals(var, val) => {
                let v = bindings.get_value(var)?;
                Some((v, Range::singleton(*val)))
            }
            _ => None,
        }
    }

    /// Resolve a value through value_to_opt_value and then check if it's a constant.
    /// Returns Some(const_val) if the value maps to a known constant.
    fn resolve_to_const(&self, value: ValueId) -> Option<i64> {
        let opt = *self.value_to_opt_value.get(&value).unwrap_or(&value);
        match self.dfg.value_defs.get(&opt).copied() {
            Some(ValueDef::Inst(iid)) => {
                let inst = &self.dfg.insts[&iid];
                if inst.opcode == Opcode::Const {
                    return inst.immediate;
                }
                None
            }
            _ => None,
        }
    }

    /// Optimize a skeleton instruction
    ///
    /// These instructions stay in the layout but can still benefit from:
    /// - GVN (if idempotent)
    /// - Alias analysis (for loads/stores)
    fn optimize_skeleton_inst(&mut self, inst_id: InstId, block: BlockId) {
        self.stats.skeleton_inst += 1;

        let inst = self.dfg.insts[&inst_id].clone();

        // Dead branch elimination: if a CondBranch condition is a known constant,
        // convert it to an unconditional Branch.
        if inst.opcode == Opcode::CondBranch {
            if let Some(BranchInfo::Conditional(then_block, then_count, else_block, else_count)) =
                inst.branch_info.clone()
            {
                if let Some(cond_val) = inst.args.first().copied() {
                    let const_val = self.resolve_to_const(cond_val).or_else(|| {
                        // Also check range: if range is [1,1] or [0,0]
                        let r = self.range_assumptions.get_range(cond_val);
                        if r.min == r.max {
                            Some(r.min)
                        } else {
                            None
                        }
                    });
                    if let Some(cv) = const_val {
                        let (target, arg_count, arg_offset) = if cv != 0 {
                            (then_block, then_count, 1usize)
                        } else {
                            (else_block, else_count, 1 + then_count)
                        };
                        let new_args: Vec<ValueId> = inst
                            .args
                            .iter()
                            .skip(arg_offset)
                            .take(arg_count)
                            .copied()
                            .collect();
                        let new_inst = Instruction::with_branch(
                            Opcode::Branch,
                            new_args,
                            inst.ty,
                            BranchInfo::Jump(target),
                        );
                        self.dfg.insts.insert(inst_id, new_inst);
                        // Map any results to themselves
                        for &result in self.dfg.inst_results(inst_id) {
                            self.value_to_opt_value.insert(result, result);
                            self.available_block.insert(result, block);
                        }
                        return;
                    }
                }
            }
        }

        // Try to GVN if the operation is idempotent
        if inst.opcode.is_mergeable() {
            let key = (inst.ty, inst.clone());

            match self.gvn_map.entry(key) {
                ScopedEntry::Occupied(entry) => {
                    if let Some(existing_value) = entry.get() {
                        // get value of the existing instruction
                        if let ValueDef::Inst(existing_inst_id) =
                            self.dfg.value_def(*existing_value)
                        {
                            if self.dfg.instructions_mergeable(inst_id, existing_inst_id) {
                                self.stats.skeleton_inst_gvn += 1;
                                let result = self.dfg.first_result(inst_id);
                                self.value_to_opt_value.insert(result, *existing_value);

                                if let Some(&avail_block) = self.available_block.get(existing_value)
                                {
                                    self.available_block.insert(result, avail_block);
                                }

                                return;
                            } else {
                            }
                        }
                    }
                }
                ScopedEntry::Vacant(entry) => {
                    // New instruction
                    let result = self.dfg.first_result(inst_id);
                    self.value_to_opt_value.insert(result, result);
                    self.available_block.insert(result, block);
                    entry.insert(Some(result));
                }
            }
        } else {
            // Non-mergeable skeleton instruction - just map results to themselves
            for &result in self.dfg.inst_results(inst_id) {
                self.value_to_opt_value.insert(result, result);
                self.available_block.insert(result, block);
            }
        }
    }

    /// Compute where a pure instruction becomes available
    ///
    /// A pure instruction is available at the highest (closest to entry)
    /// block where all its arguments are available.
    fn get_available_block(&self, inst_id: InstId) -> BlockId {
        let inst = &self.dfg.insts[&inst_id];

        // Find the deepest (furthest from entry) available block among all args
        inst.args
            .iter()
            .filter_map(|&arg| self.available_block.get(&arg).copied())
            .max_by(|&a, &b| {
                if self.domtree.block_dominates(a, b) {
                    std::cmp::Ordering::Less
                } else {
                    std::cmp::Ordering::Greater
                }
            })
            .unwrap_or_else(|| {
                // No args, so available at entry
                *self.gvn_map_blocks.first().unwrap()
            })
    }

    /// Merge availability of two values (for union nodes)
    ///
    /// The union is available at whichever block dominates the other
    fn merge_availability(&self, a: ValueId, b: ValueId) -> BlockId {
        let a_block = self.available_block[&a];
        let b_block = self.available_block[&b];

        if self.domtree.block_dominates(a_block, b_block) {
            a_block
        } else {
            b_block
        }
    }
}
