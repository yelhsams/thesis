//! Egraph pass: pull pure ops out of the CFG, GVN them and apply rewrites in
//! an egraph, then elaborate the best version back into the CFG.

use crate::pattern::*;
use crate::range::{learn_from_comparison, Range, RangeAssumptions};
use crate::rewrite_integration::*;
use crate::support::*;
use crate::types::*;
use rustc_hash::{FxHashMap, FxHashSet};

const REWRITE_LIMIT: usize = 5;

pub struct EgraphPass {
    pub dfg: DataFlowGraph,
    pub layout: Layout,
    pub domtree: DominatorTree,
    pub stats: Stats,

    rewrite_engine: RewriteEngine,

    /// original value -> optimized value, populated during the egraph build.
    gvn_mapping: FxHashMap<ValueId, ValueId>,

    pub range_assumptions: RangeAssumptions,

    /// Range facts per CFG edge populated during `remove_pure_and_optimize`
    /// and used during the domtree walk
    pub block_entry_facts: FxHashMap<(BlockId, BlockId), Vec<(ValueId, crate::range::Range)>>,

    pub cfg_preds: FxHashMap<BlockId, Vec<BlockId>>,

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
            gvn_mapping: FxHashMap::default(),
            range_assumptions: RangeAssumptions::new(),
            block_entry_facts: FxHashMap::default(),
            cfg_preds: FxHashMap::default(),
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

    pub fn run(&mut self) {
        // Single-pass aegraph build, same as Cranelift.
        // Domtree walk which now includes inline range
        // propagation, branch-condition learning, and conditional unions
        self.remove_pure_and_optimize();

        // Same as cranelift, phi-elimination convergence
        if self.path_sensitive {
            self.eliminate_redundant_params();
        }

        // Resolve GVN canonicals in instruction args before elaboration
        self.apply_gvn_to_args();

        // scoped, context-aware extraction
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
        self.global_cse_and_gcm();

        for _ in 0..5 {
            let mut changed = false;
            changed |= self.eliminate_dead_blocks();
            changed |= self.simplify_constant_brif();
            changed |= self.simplify_trivial_blocks();
            changed |= self.simplify_redundant_brif();
            changed |= self.simplify_uniform_returns();
            if !changed {
                break;
            }
        }
        self.eliminate_dead_blocks();
    }

    fn apply_gvn_to_args(&mut self) {
        let all_insts: Vec<InstId> = self.dfg.insts.keys().copied().collect();
        for inst_id in all_insts {
            // Take args out to avoid borrowing self.dfg while calling fully_resolve.
            let mut args = std::mem::take(&mut self.dfg.insts.get_mut(&inst_id).unwrap().args);
            for arg in &mut args {
                let resolved = self.fully_resolve(*arg);
                if resolved != *arg {
                    *arg = resolved;
                }
            }
            self.dfg.insts.get_mut(&inst_id).unwrap().args = args;
        }
    }

    /// Rebuild each block's instruction list to only include what's needed.
    fn rebuild_layout(&mut self) {
        for &block_id in &self.layout.blocks.clone() {
            let block_data = self.layout.block_data.get(&block_id).unwrap();
            let mut needed_values: FxHashSet<ValueId> = FxHashSet::default();
            let mut skeleton_insts = Vec::new();

            // Skeleton (impure) instructions stay; mark their args as needed.
            for &inst_id in &block_data.insts {
                let inst = &self.dfg.insts[&inst_id];

                if !inst.opcode.is_pure() {
                    skeleton_insts.push(inst_id);

                    for &arg in &inst.args {
                        needed_values.insert(arg);
                    }
                }
            }

            // Walk back from needed values to pull in all transitive pure deps.
            let mut pure_insts_set: FxHashSet<InstId> = FxHashSet::default();
            let mut work_queue: Vec<ValueId> = needed_values.iter().copied().collect();
            let mut processed: FxHashSet<ValueId> = FxHashSet::default();

            while let Some(value) = work_queue.pop() {
                if !processed.insert(value) {
                    continue;
                }

                if let ValueDef::Inst(inst_id) = self.dfg.value_def(value) {
                    let inst = &self.dfg.insts[&inst_id];

                    // Skeleton insts are already in; only collect pure ones.
                    if inst.opcode.is_pure() {
                        pure_insts_set.insert(inst_id);

                        for &arg in &inst.args {
                            if !processed.contains(&arg) {
                                work_queue.push(arg);
                            }
                        }
                    }
                }
            }

            // Pure insts in dependency order, then skeleton.
            let pure_insts_to_add = self.topological_sort_instructions(&pure_insts_set);
            let mut new_insts = pure_insts_to_add;
            new_insts.extend(skeleton_insts);

            let block = self.layout.block_data.get_mut(&block_id).unwrap();
            block.insts = new_insts;
        }
    }

    /// Sanity check: no Union nodes should be reachable from instruction
    /// args after elaboration + layout rebuild.
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

    #[cfg(not(debug_assertions))]
    fn check_no_unions(&self) {}

    /// Topologically sort instructions so defs precede uses.
    fn topological_sort_instructions(&self, inst_set: &FxHashSet<InstId>) -> Vec<InstId> {
        let mut value_to_inst: FxHashMap<ValueId, InstId> = FxHashMap::default();
        for &inst_id in inst_set {
            if let Some(result) = self.dfg.inst_results(inst_id).first() {
                value_to_inst.insert(*result, inst_id);
            }
        }

        let mut in_degree: FxHashMap<InstId, usize> = FxHashMap::default();
        let mut dependents: FxHashMap<InstId, Vec<InstId>> = FxHashMap::default();

        for &inst_id in inst_set {
            in_degree.entry(inst_id).or_insert(0);
            dependents.entry(inst_id).or_insert_with(Vec::new);

            let inst = &self.dfg.insts[&inst_id];
            for &arg in &inst.args {
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

        // Kahn's algorithm. Sort the queue for deterministic output.
        let mut result = Vec::new();
        let mut queue: Vec<InstId> = in_degree
            .iter()
            .filter(|(_, &degree)| degree == 0)
            .map(|(&inst_id, _)| inst_id)
            .collect();
        queue.sort();

        while let Some(inst_id) = queue.pop() {
            result.push(inst_id);

            if let Some(deps) = dependents.get(&inst_id) {
                for &dependent in deps {
                    if let Some(degree) = in_degree.get_mut(&dependent) {
                        *degree -= 1;
                        if *degree == 0 {
                            queue.push(dependent);
                            queue.sort();
                        }
                    }
                }
            }
        }

        result
    }

    pub fn add_rewrite_rule(&mut self, rule: Rewrite) {
        self.rewrite_engine.add_rule(rule);
    }

    /// True if `value` was remapped by GVN or redundant-phi elimination.
    pub fn is_gvn_remapped(&self, value: ValueId) -> bool {
        self.gvn_mapping.contains_key(&value)
    }

    pub fn gvn_mapping(&self) -> &FxHashMap<ValueId, ValueId> {
        &self.gvn_mapping
    }

    /// If every return in the function returns the same value (and it's
    /// available in the entry block or is a constant), replace the body with
    /// just `return value`. Returns true if it rewrote anything.
    fn simplify_uniform_returns(&mut self) -> bool {
        let entry = match self.layout.entry_block() {
            Some(b) => b,
            None => return false,
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
            return false;
        }

        let first = return_values[0];
        if !return_values.iter().all(|&v| v == first) {
            return false;
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
                return true;
            }
            return false;
        }

        let entry_data = self.layout.block_data.get_mut(&entry).unwrap();
        if let Some(&last_inst_id) = entry_data.insts.last() {
            let last = &self.dfg.insts[&last_inst_id];
            if last.branch_info.is_some() {
                let ret_inst = Instruction::new(Opcode::Return, vec![first], Type::I32);
                let ret_id = self.dfg.make_inst(ret_inst);

                let mut needed: FxHashSet<InstId> = FxHashSet::default();
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
                return true;
            }
        }
        false
    }

    /// Brif with a constant condition becomes an unconditional branch.
    fn simplify_constant_brif(&mut self) -> bool {
        let mut changed = false;
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
                    changed = true;
                }
            }
        }
        changed
    }

    /// Drop unreachable blocks.
    fn eliminate_dead_blocks(&mut self) -> bool {
        let entry = match self.layout.entry_block() {
            Some(b) => b,
            None => return false,
        };

        let mut reachable: FxHashSet<BlockId> = FxHashSet::default();
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

        let before = self.layout.blocks.len();
        self.layout.blocks.retain(|b| reachable.contains(b));
        self.layout.blocks.len() != before
    }

    /// Forward branches through single-instruction blocks (just a return or
    /// just a jump).
    fn simplify_trivial_blocks(&mut self) -> bool {
        let mut any_changed = false;
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

                // Block with just a return — inline into predecessors.
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

                // Block with just a jump — forward predecessors to its target.
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
            any_changed |= changed;
        }
        any_changed
    }

    /// If both arms of a brif go to the same block with the same args, turn
    /// it into a jump.
    fn simplify_redundant_brif(&mut self) -> bool {
        let mut changed = false;
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
                        changed = true;
                    }
                }
            }
        }
        changed
    }

    /// Redundant phi elimination. Builds a fresh param→sources map from the
    /// layout and drops any block param whose preds all pass the same
    /// canonical value. Iterates to a fixed point.
    fn eliminate_redundant_params(&mut self) {
        let mut param_sources: FxHashMap<ValueId, Vec<ValueId>> = FxHashMap::default();
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
                                        .push(incoming);
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
                                    param_sources.entry(param).or_default().push(incoming);
                                }
                            }
                        }
                        if let Some(eb) = self.layout.block_data.get(&else_block) {
                            for i in 0..else_count {
                                if let (Some(&param), Some(&incoming)) =
                                    (eb.params.get(i), inst.args.get(1 + then_count + i))
                                {
                                    param_sources.entry(param).or_default().push(incoming);
                                }
                            }
                        }
                    }
                    None => {}
                }
            }
        }

        // Loop to fixed point — a new gvn_mapping entry can make a previously
        // non-redundant param redundant.
        let mut changed = true;
        while changed {
            changed = false;
            for (&param, sources) in &param_sources {
                if sources.is_empty() {
                    continue;
                }
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

                let mut first: Option<ValueId> = None;
                let mut uniform = true;
                for &incoming in sources {
                    let resolved = self.fully_resolve(incoming);
                    if resolved == param {
                        continue; // self-reference from a loop
                    }
                    match first {
                        None => first = Some(resolved),
                        Some(f) if f == resolved => {}
                        _ => {
                            uniform = false;
                            break;
                        }
                    }
                }

                if !uniform {
                    continue;
                }
                let Some(first) = first else {
                    continue;
                };

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

    /// Follow `gvn_mapping` transitively. Depth-limited so unexpected cycles
    /// don't loop forever.
    pub fn fully_resolve(&self, value: ValueId) -> ValueId {
        let mut current = value;
        for _ in 0..64 {
            match self.gvn_mapping.get(&current).copied() {
                Some(next) if next != current => current = next,
                _ => return current,
            }
        }
        current
    }

    /// Pull pure ops out of the layout and build the egraph.
    ///
    /// Single domtree-preorder walk that handles block-param incoming maps,
    /// branch-condition recording, range propagation, union creation, and
    /// rewrites all inline.
    fn remove_pure_and_optimize(&mut self) {
        let mut value_to_opt_value: FxHashMap<ValueId, ValueId> = FxHashMap::default();

        let mut gvn_map: ScopedHashMap<(Type, Instruction), Option<ValueId>> =
            ScopedHashMap::with_capacity(100);

        let mut available_block: FxHashMap<ValueId, BlockId> = FxHashMap::default();

        let mut gvn_map_blocks: Vec<BlockId> = Vec::new();

        // Pre-scan (target, param_idx) → [(pred, incoming_value)]. Values are
        // unrewritten; resolved through value_to_opt_value at read time.
        // Must be precomputed since CFG preds aren't guaranteed to come
        // before successors in domtree preorder (e.g. merge blocks).
        let mut block_param_incoming: FxHashMap<(BlockId, usize), Vec<(BlockId, ValueId)>> =
            FxHashMap::default();
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

        // CFG preds, used for sound range-fact joining at merges.
        let mut cfg_preds: FxHashMap<BlockId, Vec<BlockId>> = FxHashMap::default();
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

        // Range facts per edge, populated inline during the walk.
        let mut block_entry_facts: FxHashMap<(BlockId, BlockId), Vec<(ValueId, Range)>> =
            FxHashMap::default();
        let mut block_edge_conditions: FxHashMap<
            (BlockId, BlockId),
            Vec<(ValueId, Option<i64>, Opcode, bool)>,
        > = FxHashMap::default();

        let root = self.layout.entry_block().unwrap();

        enum StackEntry {
            Visit(BlockId),
            Pop,
        }

        let mut block_stack = vec![StackEntry::Visit(root)];
        // Scratch buffers reused across iterations.
        let mut arg_ranges_buf: Vec<crate::range::Range> = Vec::with_capacity(8);
        let mut cond_values_buf: Vec<ValueId> = Vec::with_capacity(8);

        while let Some(entry) = block_stack.pop() {
            match entry {
                StackEntry::Visit(block) => {
                    block_stack.push(StackEntry::Pop);
                    // Reverse so lower block IDs come first, which means
                    // merge blocks see their preds' value_to_opt_value.
                    for &child in self.domtree.children(block).iter().rev() {
                        block_stack.push(StackEntry::Visit(child));
                    }

                    gvn_map.increment_depth();
                    gvn_map_blocks.push(block);
                    self.range_assumptions.push_scope();

                    let block_data = self.layout.block_data[&block].clone();

                    // 1. Apply range facts from incoming edges. At a merge,
                    //    intersect within an edge then union across edges —
                    //    intersecting contradictory facts would be unsound.
                    if self.path_sensitive {
                        let preds = cfg_preds.get(&block).cloned().unwrap_or_default();
                        let num_edges = preds.len();
                        let mut value_edge_count: FxHashMap<ValueId, usize> = FxHashMap::default();
                        let mut value_union: FxHashMap<ValueId, Range> = FxHashMap::default();
                        for pred in &preds {
                            if let Some(facts) = block_entry_facts.get(&(*pred, block)) {
                                // Intersect facts about the same value on this edge.
                                let mut per_edge: FxHashMap<ValueId, Range> = FxHashMap::default();
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
                                // Union across edges.
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
                        // Only apply facts that appear on every incoming edge.
                        for (value, range) in &value_union {
                            if value_edge_count.get(value) == Some(&num_edges)
                                && !range.is_unbounded()
                            {
                                self.range_assumptions.assume_range(*value, *range);
                            }
                        }
                    }

                    // Mark nonzero only if every incoming edge agrees.
                    if self.path_sensitive {
                        let preds = cfg_preds.get(&block).cloned().unwrap_or_default();
                        let num_preds = preds.len();
                        if num_preds > 0 {
                            let mut nonzero_edge_count: FxHashMap<ValueId, usize> =
                                FxHashMap::default();
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

                    // 2. Block params: identity-map, per-edge range join,
                    //    singleton-constant substitution.
                    for (param_idx, &param) in block_data.params.iter().enumerate() {
                        value_to_opt_value.insert(param, param);
                        available_block.insert(param, block);

                        if self.path_sensitive {
                            let key = (block, param_idx);
                            if let Some(incoming) = block_param_incoming.get(&key) {
                                let mut joined = Range::empty();
                                for &(pred, src_val) in incoming {
                                    if pred == block {
                                        continue;
                                    }
                                    let mut src_range = self.range_assumptions.get_range(src_val);
                                    // Narrow by edge-specific conditions.
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

                            // If the joined range is a singleton, substitute the constant.
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
                        // Block-param equivalence: if every non-self incoming
                        // is the same value (or all the same constant), make
                        // the param an alias of it.
                        for (param_idx, &param) in block_data.params.iter().enumerate() {
                            let incoming_key = (block, param_idx);
                            let incoming = match block_param_incoming.get(&incoming_key) {
                                Some(s) if !s.is_empty() => s.clone(),
                                _ => continue,
                            };

                            // Resolve each incoming through value_to_opt_value.
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
                            }
                            // No conditional union for the disagreement case:
                            // unconditional-jump edges carry no range facts,
                            // so the union's assumption set would be empty
                            // and treated as globally valid. That's unsound
                            // (v_param only equals one incoming per path) and
                            // it tanks best-cost computation by exposing the
                            // phi as a Cost::ZERO equivalent, blocking range-
                            // conditioned rewrites like sdiv→ushr.
                        }
                    }

                    // OptimizeCtx is hoisted out of the per-instruction loop
                    // (and reused by the cond_values pass below) so we don't
                    // re-borrow self each time. Per-iteration state on ctx
                    // gets reset at the top of each loop iteration.
                    let path_sensitive = self.path_sensitive;
                    let mut ctx = OptimizeCtx {
                        dfg: &mut self.dfg,
                        value_to_opt_value: &mut value_to_opt_value,
                        gvn_map: &mut gvn_map,
                        gvn_map_blocks: &gvn_map_blocks,
                        available_block: &mut available_block,
                        stats: &mut self.stats,
                        domtree: &self.domtree,
                        rewrite_depth: 0,
                        subsume_values: FxHashSet::default(),
                        rewrite_engine: &mut self.rewrite_engine,
                        range_assumptions: &mut self.range_assumptions,
                        path_sensitive,
                    };

                    for &inst_id in &block_data.insts {
                        ctx.rewrite_depth = 0;
                        ctx.subsume_values.clear();

                        // Rewrite args via value_to_opt_value and grab the
                        // opcode for dispatch in a single pass.
                        let opcode = {
                            let inst_ref = ctx.dfg.insts.get_mut(&inst_id).unwrap();
                            for arg in &mut inst_ref.args {
                                if let Some(&opt_value) = ctx.value_to_opt_value.get(arg) {
                                    *arg = opt_value;
                                }
                            }
                            inst_ref.opcode
                        };

                        if opcode.is_pure() {
                            ctx.insert_pure_enode(inst_id);
                        } else {
                            ctx.optimize_skeleton_inst(inst_id, block);
                        }

                        // Forward range propagation. Reuse the scratch buffer.
                        if path_sensitive {
                            arg_ranges_buf.clear();
                            let (opcode_after, immediate_after) = {
                                let inst_ref = &ctx.dfg.insts[&inst_id];
                                for &arg in &inst_ref.args {
                                    arg_ranges_buf.push(ctx.range_assumptions.get_range(arg));
                                }
                                (inst_ref.opcode, inst_ref.immediate)
                            };
                            let output_range = crate::range::compute_inst_range(
                                opcode_after,
                                &arg_ranges_buf,
                                immediate_after,
                            );
                            if !output_range.is_unbounded() {
                                if let Some(&result) = ctx.dfg.inst_results(inst_id).first() {
                                    let opt_result = ctx
                                        .value_to_opt_value
                                        .get(&result)
                                        .copied()
                                        .unwrap_or(result);
                                    ctx.range_assumptions.assume_range(result, output_range);
                                    ctx.stats.ranges_propagated += 1;
                                    if opt_result != result {
                                        ctx.range_assumptions
                                            .assume_range(opt_result, output_range);
                                    }
                                    if let Some(c) = output_range.as_singleton() {
                                        let current = ctx
                                            .value_to_opt_value
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
                                                ctx.gvn_map.get(&key)
                                            {
                                                *existing
                                            } else {
                                                let const_inst_id = ctx.dfg.make_inst(const_inst);
                                                let cv = ctx
                                                    .dfg
                                                    .make_inst_result(const_inst_id, Type::I32);
                                                ctx.gvn_map.insert(key, Some(cv));
                                                ctx.available_block.insert(cv, block);
                                                cv
                                            };
                                            let _ = ctx.dfg.make_union(opt_result, const_val);
                                            ctx.value_to_opt_value.insert(result, const_val);
                                            ctx.stats.union += 1;
                                        }
                                    }
                                }
                            }
                        }

                        // Record branch conditions inline (after arg rewriting
                        // so they reference optimized values).
                        if path_sensitive {
                            let cb_info = {
                                let inst_ref = &ctx.dfg.insts[&inst_id];
                                if inst_ref.opcode == Opcode::CondBranch {
                                    if let Some(BranchInfo::Conditional(then_b, _, else_b, _)) =
                                        inst_ref.branch_info
                                    {
                                        if !inst_ref.args.is_empty() {
                                            Some((then_b, else_b, inst_ref.args[0]))
                                        } else {
                                            None
                                        }
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            };
                            if let Some((then_b, else_b, cond_value)) = cb_info {
                                // True branch: cond is nonzero. Walk back
                                // through and/or/cmp to learn leaf range facts.
                                {
                                    ctx.range_assumptions.push_scope();
                                    let then_facts =
                                        ctx.range_assumptions.refine_nonzero(cond_value, &*ctx.dfg);
                                    ctx.range_assumptions.pop_scope();

                                    block_entry_facts
                                        .entry((block, then_b))
                                        .or_default()
                                        .extend(then_facts.iter().cloned());
                                    // Also record leaf facts as edge conditions
                                    // so block-param join can narrow per-edge
                                    // ranges. Approximated as a (Sge min,
                                    // Slt max+1) pair.
                                    for &(value, range) in &then_facts {
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
                                        // Fallback: no leaves, record cond != 0.
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

                                // Else branch: cond is zero.
                                {
                                    ctx.range_assumptions.push_scope();
                                    let else_facts =
                                        ctx.range_assumptions.refine_zero(cond_value, &*ctx.dfg);
                                    ctx.range_assumptions.pop_scope();

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

                    // Re-run conditional rewrites on brif condition values
                    // that were GVN-mapped from a dominator block — their
                    // original visit didn't have this block's tightened range
                    // scope, so e.g. `icmp.sge x, 0 → 1` couldn't fire.
                    // Restricted to brif conds to keep the cost bounded.
                    if path_sensitive {
                        cond_values_buf.clear();
                        for &iid in &block_data.insts {
                            let inst = &ctx.dfg.insts[&iid];
                            if inst.opcode == Opcode::CondBranch {
                                if let Some(&cond) = inst.args.first() {
                                    cond_values_buf.push(cond);
                                }
                            }
                        }
                        for &cond in &cond_values_buf {
                            let canonical =
                                ctx.value_to_opt_value.get(&cond).copied().unwrap_or(cond);
                            let defined_in_dominator = ctx
                                .available_block
                                .get(&canonical)
                                .copied()
                                .map(|b| b != block)
                                .unwrap_or(false);
                            if !defined_in_dominator {
                                continue;
                            }
                            ctx.rewrite_depth = 0;
                            ctx.subsume_values.clear();
                            ctx.apply_conditional_rewrites_and_store(canonical);
                        }
                    }

                    // Release borrows on self before the next iteration.
                    drop(ctx);
                }

                StackEntry::Pop => {
                    gvn_map.decrement_depth();
                    gvn_map_blocks.pop();
                    self.range_assumptions.pop_scope();
                }
            }
        }
        // Hand off to the elaborator.
        self.gvn_mapping = value_to_opt_value;
        self.block_entry_facts = block_entry_facts;
        self.cfg_preds = cfg_preds;
    }

    /// Global CSE + global code motion, run after scoped-GVN elaboration.
    /// Dedupes structurally-equal pure insts across the whole CFG, then
    /// hoists each canonical inst to the LCA of its users. Recovers
    /// equivalences that dominator-local GVN misses (e.g. both arms of a
    /// diamond computing the same expression).
    fn global_cse_and_gcm(&mut self) {
        // Domtree preorder over reachable blocks. Used both for CSE
        // canonicalization (prefer dominating defs) and as iteration order.
        let entry = match self.layout.entry_block() {
            Some(b) => b,
            None => return,
        };
        let preorder: Vec<BlockId> = {
            let mut result = Vec::new();
            let mut stack = vec![entry];
            while let Some(b) = stack.pop() {
                if !self.layout.block_data.contains_key(&b) {
                    continue;
                }
                result.push(b);
                for &c in self.domtree.children(b).iter().rev() {
                    stack.push(c);
                }
            }
            result
        };
        let in_layout: FxHashSet<BlockId> = preorder.iter().copied().collect();

        // ----- Phase 1: global CSE, looped to fixed point -----
        // Each iteration keys pure insts by (opcode, type, imm, resolved args)
        // and dedupes against earlier ones. Canonicalizing args exposes more
        // equivalences, hence the loop.
        let mut value_map: FxHashMap<ValueId, ValueId> = FxHashMap::default();
        fn resolve(vm: &FxHashMap<ValueId, ValueId>, mut v: ValueId) -> ValueId {
            let mut guard = 0usize;
            while let Some(&next) = vm.get(&v) {
                if next == v || guard > 1024 {
                    break;
                }
                v = next;
                guard += 1;
            }
            v
        }

        for _iter in 0..16 {
            let mut changed = false;
            let mut by_key: FxHashMap<(Opcode, Type, Option<i64>, Vec<ValueId>), ValueId> =
                FxHashMap::default();
            for &block_id in &preorder {
                let block_insts = self.layout.block_data[&block_id].insts.clone();
                for inst_id in block_insts {
                    let inst = self.dfg.insts[&inst_id].clone();
                    if !inst.opcode.is_pure() {
                        continue;
                    }
                    let result = match self.dfg.inst_results(inst_id).first().copied() {
                        Some(r) => r,
                        None => continue,
                    };
                    // Already canonicalized to someone else — skip; its
                    // defining inst is now redundant.
                    if let Some(&c) = value_map.get(&result) {
                        if c != result {
                            continue;
                        }
                    }
                    let resolved_args: Vec<ValueId> =
                        inst.args.iter().map(|&a| resolve(&value_map, a)).collect();
                    let key = (inst.opcode, inst.ty, inst.immediate, resolved_args);
                    if let Some(&canonical) = by_key.get(&key) {
                        if canonical != result {
                            value_map.insert(result, canonical);
                            changed = true;
                        }
                    } else {
                        by_key.insert(key, result);
                    }
                }
            }
            if !changed {
                break;
            }
        }

        // ----- Phase 2: rewrite every inst's args via value_map -----
        let all_insts: Vec<InstId> = self.dfg.insts.keys().copied().collect();
        for inst_id in all_insts {
            let mut inst = self.dfg.insts[&inst_id].clone();
            let mut changed = false;
            for arg in &mut inst.args {
                let canonical = resolve(&value_map, *arg);
                if canonical != *arg {
                    *arg = canonical;
                    changed = true;
                }
            }
            if changed {
                self.dfg.insts.insert(inst_id, inst);
            }
        }

        // ----- Phase 3: drop redundant pure insts from block lists -----
        for &block_id in &preorder {
            let block = self.layout.block_data.get_mut(&block_id).unwrap();
            block.insts.retain(|&iid| {
                let inst = &self.dfg.insts[&iid];
                if !inst.opcode.is_pure() {
                    return true;
                }
                let result = match self.dfg.inst_results(iid).first().copied() {
                    Some(r) => r,
                    None => return true,
                };
                // Keep only the canonical rep of each equivalence class.
                resolve(&value_map, result) == result
            });
        }

        // ----- Phase 4: global code motion -----
        //
        // For each pure inst, compute target = LCA of its users' blocks in
        // the domtree. Loop to fixed point so hoisting a consumer also pulls
        // its pure-inst args up (hoisting an `iadd` to a diamond's dominator
        // pulls the `iconst` along too). Then apply moves only when the
        // target strictly hoists and every arg's def block dominates the new
        // placement; otherwise leave it and let `finalize_gcm_validity` clone
        // the arg locally.
        //
        // `rebuild_layout` can place the same InstId in multiple block lists
        // (any block using a value pulls in its defining pure inst). For GCM
        // we want each inst's "home" to be the dominating placement, else
        // the move phase would shuffle it pointlessly and break the
        // duplicate references the CFG cleanups rely on. Walking in domtree
        // preorder + `or_insert` picks the highest block.
        let mut inst_block: FxHashMap<InstId, BlockId> = FxHashMap::default();
        for &block_id in &preorder {
            for &iid in &self.layout.block_data[&block_id].insts {
                inst_block.entry(iid).or_insert(block_id);
            }
        }

        // value -> defining InstId. Stable across the iteration since
        // neither CSE nor GCM creates new insts.
        let mut def_of: FxHashMap<ValueId, InstId> = FxHashMap::default();
        for (&iid, _) in &inst_block {
            for &r in self.dfg.inst_results(iid) {
                def_of.insert(r, iid);
            }
        }

        // Per-inst user list: pure-inst users (whose target participates in
        // LCA propagation) vs skeleton users pinned to a block. Built once
        // from the post-CSE layout and reused across GCM iterations.
        enum UserKind {
            PureInst(InstId),
            FixedBlock(BlockId),
        }
        let mut users_of: FxHashMap<InstId, Vec<UserKind>> = FxHashMap::default();
        for &block_id in &preorder {
            let block = &self.layout.block_data[&block_id];
            for &user_id in &block.insts {
                let user = &self.dfg.insts[&user_id];
                let user_is_pure = user.opcode.is_pure();
                for &arg in &user.args {
                    if let Some(&def_id) = def_of.get(&arg) {
                        if def_id == user_id {
                            continue;
                        }
                        let entry = users_of.entry(def_id).or_default();
                        if user_is_pure {
                            entry.push(UserKind::PureInst(user_id));
                        } else {
                            entry.push(UserKind::FixedBlock(block_id));
                        }
                    }
                }
            }
        }

        // Start with target = current block for each pure inst.
        let mut target: FxHashMap<InstId, BlockId> = FxHashMap::default();
        for (&iid, &b) in &inst_block {
            if self.dfg.insts[&iid].opcode.is_pure() {
                target.insert(iid, b);
            }
        }

        // Loop to fixed point. LCA only moves targets up (closer to entry),
        // and depth is bounded, so this terminates.
        for _iter in 0..64 {
            let mut changed = false;
            // Reverse preorder: consumers settle before their args.
            let pure_list: Vec<InstId> = preorder
                .iter()
                .rev()
                .flat_map(|&b| self.layout.block_data[&b].insts.clone())
                .filter(|&iid| self.dfg.insts[&iid].opcode.is_pure())
                .collect();
            for inst_id in pure_list {
                let Some(users) = users_of.get(&inst_id) else {
                    continue;
                };
                if users.is_empty() {
                    continue;
                }
                // User blocks (using current targets for pure users).
                let mut iter = users.iter().filter_map(|uk| match uk {
                    UserKind::PureInst(iid) => target.get(iid).copied(),
                    UserKind::FixedBlock(b) => Some(*b),
                });
                let first = match iter.next() {
                    Some(b) => b,
                    None => continue,
                };
                let mut lca = first;
                let mut ok = true;
                for b in iter {
                    match self.domtree.lca(lca, b) {
                        Some(l) => lca = l,
                        None => {
                            ok = false;
                            break;
                        }
                    }
                }
                if !ok || !in_layout.contains(&lca) {
                    continue;
                }
                let entry_target = target.entry(inst_id).or_insert(lca);
                if *entry_target != lca {
                    *entry_target = lca;
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }

        // Apply targets. For each inst, every arg's source block must
        // dominate the target. If not (e.g. CSE canonicalized across a
        // diamond and the arg couldn't be hoisted), skip the move — the
        // duplication pass below will handle it.
        for (&inst_id, &new_block) in target.clone().iter() {
            let current = match inst_block.get(&inst_id).copied() {
                Some(b) => b,
                None => continue,
            };
            if new_block == current {
                continue;
            }
            if !self.domtree.block_dominates(new_block, current) {
                continue; // hoist only, never sink
            }
            let inst = self.dfg.insts[&inst_id].clone();
            let mut valid = true;
            for &arg in &inst.args {
                let src = match self.dfg.value_def(arg) {
                    ValueDef::BlockParam(b, _) => Some(b),
                    ValueDef::Inst(def_id) => target
                        .get(&def_id)
                        .copied()
                        .or_else(|| inst_block.get(&def_id).copied()),
                    ValueDef::Union(_, _) => None,
                };
                if let Some(sb) = src {
                    if !self.domtree.block_dominates(sb, new_block) {
                        valid = false;
                        break;
                    }
                }
            }
            if !valid {
                continue;
            }
            let cur_block = self.layout.block_data.get_mut(&current).unwrap();
            cur_block.insts.retain(|&i| i != inst_id);
            self.layout
                .block_data
                .get_mut(&new_block)
                .unwrap()
                .insts
                .push(inst_id);
            inst_block.insert(inst_id, new_block);
        }

        // Restore SSA dominance: if any arg's defining inst is in a non-
        // dominating block (because CSE merged across a diamond and GCM
        // couldn't hoist it), clone the def into the user's block. Iterate
        // since fresh duplicates may have the same problem.
        for _ in 0..16 {
            let mut changed = false;
            let blocks_snapshot: Vec<BlockId> = preorder.clone();
            for &block_id in &blocks_snapshot {
                let block_insts = self.layout.block_data[&block_id].insts.clone();
                for user_id in block_insts {
                    let user = self.dfg.insts[&user_id].clone();
                    let mut new_args = user.args.clone();
                    let mut any_fix = false;
                    for arg in new_args.iter_mut() {
                        if let ValueDef::Inst(def_id) = self.dfg.value_def(*arg) {
                            if !self.dfg.insts[&def_id].opcode.is_pure() {
                                continue;
                            }
                            let def_block = match inst_block.get(&def_id).copied() {
                                Some(b) => b,
                                None => continue,
                            };
                            if self.domtree.block_dominates(def_block, block_id) {
                                continue;
                            }
                            // Clone the def into this block. Topo sort fixes order.
                            let orig = self.dfg.insts[&def_id].clone();
                            let new_inst_id = self.dfg.make_inst(orig.clone());
                            let new_result = self.dfg.make_inst_result(new_inst_id, orig.ty);
                            self.layout
                                .block_data
                                .get_mut(&block_id)
                                .unwrap()
                                .insts
                                .push(new_inst_id);
                            inst_block.insert(new_inst_id, block_id);
                            for &r in self.dfg.inst_results(new_inst_id) {
                                def_of.insert(r, new_inst_id);
                            }
                            *arg = new_result;
                            any_fix = true;
                            changed = true;
                        }
                    }
                    if any_fix {
                        let mut u = self.dfg.insts[&user_id].clone();
                        u.args = new_args;
                        self.dfg.insts.insert(user_id, u);
                    }
                }
            }
            if !changed {
                break;
            }
        }

        // ----- Phase 5: topo-sort each block's inst list -----
        for &block_id in &preorder {
            let sorted = self.topo_sort_block_insts(block_id);
            self.layout.block_data.get_mut(&block_id).unwrap().insts = sorted;
        }

        // ----- Phase 6: drop pure insts with no users -----
        loop {
            let mut used: FxHashSet<ValueId> = FxHashSet::default();
            for &block_id in &preorder {
                for &iid in &self.layout.block_data[&block_id].insts {
                    for &arg in &self.dfg.insts[&iid].args {
                        used.insert(arg);
                    }
                }
            }
            let mut removed_any = false;
            for &block_id in &preorder {
                let insts = self.layout.block_data[&block_id].insts.clone();
                let mut keep = Vec::with_capacity(insts.len());
                for iid in insts {
                    let inst = &self.dfg.insts[&iid];
                    if !inst.opcode.is_pure() {
                        keep.push(iid);
                        continue;
                    }
                    let result = self.dfg.inst_results(iid).first().copied();
                    if result.map(|r| used.contains(&r)).unwrap_or(true) {
                        keep.push(iid);
                    } else {
                        removed_any = true;
                    }
                }
                self.layout.block_data.get_mut(&block_id).unwrap().insts = keep;
            }
            if !removed_any {
                break;
            }
        }
    }

    /// Topo-sort the insts in a block so defs come before uses. Terminator
    /// is pinned last.
    fn topo_sort_block_insts(&self, block_id: BlockId) -> Vec<InstId> {
        let block = &self.layout.block_data[&block_id];
        if block.insts.is_empty() {
            return Vec::new();
        }
        let inst_set: FxHashSet<InstId> = block.insts.iter().copied().collect();

        let terminator = block
            .insts
            .iter()
            .rev()
            .find(|&&iid| {
                let inst = &self.dfg.insts[&iid];
                inst.opcode.is_terminator() || inst.branch_info.is_some()
            })
            .copied();

        let mut def_map: FxHashMap<ValueId, InstId> = FxHashMap::default();
        for &iid in &block.insts {
            for &r in self.dfg.inst_results(iid) {
                def_map.insert(r, iid);
            }
        }

        let mut in_deg: FxHashMap<InstId, usize> = FxHashMap::default();
        let mut dependents: FxHashMap<InstId, Vec<InstId>> = FxHashMap::default();
        for &iid in &block.insts {
            in_deg.insert(iid, 0);
        }
        for &iid in &block.insts {
            let inst = &self.dfg.insts[&iid];
            for &arg in &inst.args {
                if let Some(&def_id) = def_map.get(&arg) {
                    if def_id != iid && inst_set.contains(&def_id) {
                        dependents.entry(def_id).or_default().push(iid);
                        *in_deg.get_mut(&iid).unwrap() += 1;
                    }
                }
            }
        }

        // Kahn's algorithm. Pop smallest InstId first for determinism and
        // defer the terminator until everything else is scheduled.
        let mut ready: Vec<InstId> = in_deg
            .iter()
            .filter(|(&iid, &d)| d == 0 && Some(iid) != terminator)
            .map(|(&iid, _)| iid)
            .collect();
        ready.sort_by(|a, b| b.0.cmp(&a.0));

        let mut result: Vec<InstId> = Vec::with_capacity(block.insts.len());
        while let Some(iid) = ready.pop() {
            result.push(iid);
            if let Some(deps) = dependents.get(&iid) {
                for &dep in deps {
                    let d = in_deg.get_mut(&dep).unwrap();
                    *d -= 1;
                    if *d == 0 && Some(dep) != terminator {
                        ready.push(dep);
                    }
                }
                ready.sort_by(|a, b| b.0.cmp(&a.0));
            }
        }

        if let Some(t) = terminator {
            result.push(t);
        }
        result
    }
}

/// Shared mutable context for the per-instruction optimization loop.
struct OptimizeCtx<'a> {
    dfg: &'a mut DataFlowGraph,
    value_to_opt_value: &'a mut FxHashMap<ValueId, ValueId>,
    gvn_map: &'a mut ScopedHashMap<(Type, Instruction), Option<ValueId>>,
    gvn_map_blocks: &'a Vec<BlockId>,
    available_block: &'a mut FxHashMap<ValueId, BlockId>,
    stats: &'a mut Stats,
    domtree: &'a DominatorTree,
    rewrite_depth: usize,
    subsume_values: FxHashSet<ValueId>,
    rewrite_engine: &'a mut RewriteEngine,
    range_assumptions: &'a mut RangeAssumptions,
    path_sensitive: bool,
}

impl<'a> OptimizeCtx<'a> {
    /// Insert a pure inst into the egraph: GVN-dedupe, then apply rewrites
    /// and create unions for any equivalent forms.
    fn insert_pure_enode(&mut self, inst_id: InstId) -> ValueId {
        self.stats.pure_inst += 1;

        let inst = self.dfg.insts[&inst_id].clone();
        let key = (inst.ty, inst.clone());

        let result = self.dfg.first_result(inst_id);
        let avail_block = self.get_available_block(inst_id);

        match self.gvn_map.entry(key) {
            ScopedEntry::Occupied(entry) => {
                self.stats.pure_inst_deduped += 1;

                if let Some(existing_value) = entry.get() {
                    self.value_to_opt_value.insert(result, *existing_value);

                    if let Some(&avail_block) = self.available_block.get(existing_value) {
                        self.available_block.insert(result, avail_block);
                    }

                    return *existing_value;
                }

                // Shouldn't reach here, but be safe.
                self.value_to_opt_value.insert(result, result);
                result
            }

            ScopedEntry::Vacant(entry) => {
                self.stats.pure_inst_insert_new += 1;

                self.value_to_opt_value.insert(result, result);
                self.available_block.insert(result, avail_block);

                entry.insert(Some(result));

                // Cache opcode; apply_rewrites_and_union mutates state.
                let inst_opcode = inst.opcode;

                let opt_result = self.apply_rewrites_and_union(result);

                // Second pass for range-conditioned rewrites. Skip if no
                // range-conditioned rule can possibly match this opcode.
                if self.path_sensitive
                    && (self.rewrite_engine.has_unconstrained_range_rule
                        || self
                            .rewrite_engine
                            .range_rule_opcodes
                            .contains(&inst_opcode))
                {
                    self.apply_conditional_rewrites_and_store(result);
                }

                opt_result
            }
        }
    }

    /// Apply rewrite rules and create union nodes for the results.
    fn apply_rewrites_and_union(&mut self, value: ValueId) -> ValueId {
        if self.rewrite_depth >= REWRITE_LIMIT {
            self.stats.rewrite_depth_limit += 1;
            return value;
        }

        self.rewrite_depth += 1;
        self.stats.rewrite_rule_invoked += 1;

        let mut union_value = value;

        // Range-conditioned rewrites are handled separately in
        // apply_conditional_rewrites_and_store, which picks unconditional vs
        // conditional unions based on whether the facts are globally provable.
        // Passing range assumptions here would unsoundly create unconditional
        // unions from branch-local facts (the extractor follows unions
        // without checking range guards).
        let rewrites = self
            .rewrite_engine
            .apply_rewrites_with_ranges(self.dfg, value, None);

        for rewritten in rewrites {
            self.stats.rewrite_rule_results += 1;

            // Recursively optimize the rewritten value.
            let opt_value = if let ValueDef::Inst(rewritten_inst) = self.dfg.value_def(rewritten) {
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

            // If the rewritten value is simpler (constant or block param),
            // update value_to_opt_value so later arg rewrites see it instead
            // of the union node.
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

            let new_union = self.dfg.make_union(union_value, opt_value);
            self.stats.union += 1;

            // Availability is the block that dominates both.
            let avail = self.merge_availability(union_value, opt_value);
            self.available_block.insert(new_union, avail);

            union_value = new_union;
        }

        self.rewrite_depth -= 1;
        union_value
    }

    /// True when the current range for `value` is tighter than the root
    /// scope's — i.e., it relies on a branch-local assumption.
    fn is_branch_local(&self, value: ValueId, range: &Range) -> bool {
        if self.range_assumptions.depth() == 0 {
            return false;
        }
        let root = self.range_assumptions.root_range(value);
        !(root.min >= range.min && root.max <= range.max)
    }

    /// Drive every range-conditioned rewrite. For each rule whose range
    /// conditions hold, create an unconditional union if the facts are
    /// globally provable, or a conditional union (guarded by the branch-
    /// local facts) otherwise.
    fn apply_conditional_rewrites_and_store(&mut self, value: ValueId) {
        let defining_inst = match self.dfg.value_def(value) {
            ValueDef::Inst(id) => id,
            _ => return,
        };

        if self.rewrite_engine.range_rule_indices.is_empty() {
            return;
        }

        // Filter by the defining opcode: rules with a concrete mismatched
        // top-level opcode can't match, skip them without running the matcher.
        let defining_opcode = self.dfg.insts[&defining_inst].opcode;

        let ra_ptr = &*self.range_assumptions as *const RangeAssumptions;
        // SAFETY: only `self.dfg` is mutated below; the pointee is not
        // touched through this pointer while the shared ref is live.
        let ra = unsafe { &*ra_ptr };

        // Move `rules` out so we can iterate without cloning the whole
        // library — this is called per pure inst in path-sensitive mode and
        // the clone was a major hotspot.
        let rules = std::mem::take(&mut self.rewrite_engine.library.rules);

        for &(rule_idx, top_op) in &self.rewrite_engine.range_rule_indices {
            if let Some(op) = top_op {
                if op != defining_opcode {
                    continue;
                }
            }
            let rule = &rules[rule_idx];
            let range_conditions = Self::collect_range_conditions(&rule.conditions);
            if range_conditions.is_empty() {
                continue;
            }

            let matcher = PatternMatcher::new(self.dfg);
            let mut bindings = match matcher.match_pattern(&rule.lhs, value) {
                Some(b) => b,
                None => continue,
            };
            bindings.set_range_assumptions(ra);

            if !rule.check_conditions(&bindings) {
                continue;
            }

            // Split range facts into branch-local vs globally provable.
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

            let mut applier = PatternApplier::new(self.dfg);
            let new_value = match applier.apply_pattern(&rule.rhs, &bindings) {
                Some(v) if v != value => v,
                _ => continue,
            };

            if all_global {
                // Safe to create an unconditional union.
                let union_node = self.dfg.make_union(value, new_value);
                self.stats.union += 1;

                // Surface simpler results so the extractor sees them.
                let is_simpler = match self.dfg.value_defs.get(&new_value).copied() {
                    Some(ValueDef::Inst(iid)) => self.dfg.insts[&iid].opcode == Opcode::Const,
                    Some(ValueDef::BlockParam(_, _)) => true,
                    _ => false,
                };
                if is_simpler {
                    self.value_to_opt_value.insert(value, new_value);
                }

                if !self.available_block.contains_key(&new_value) {
                    if let Some(&blk) = self.available_block.get(&value) {
                        self.available_block.insert(new_value, blk);
                    }
                }
                let avail = self.merge_availability(value, new_value);
                self.available_block.insert(union_node, avail);
                continue;
            }

            // Branch-local facts — guard the union with them.
            let assumption_set = AssumptionSet {
                assumptions: branch_local_facts,
            };

            let _ = self
                .dfg
                .make_conditional_union(value, new_value, assumption_set);
        }

        self.rewrite_engine.library.rules = rules;
    }

    /// Flatten And-nested range conditions into a single list.
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
            }
        }
        result
    }

    /// Extract the (ValueId, required Range) implied by a range condition.
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

    /// Resolve through value_to_opt_value and return the constant if any.
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

    /// Skeleton (impure) inst optimizations: dead-branch elimination plus
    /// GVN for mergeable opcodes. These stay in the layout.
    fn optimize_skeleton_inst(&mut self, inst_id: InstId, block: BlockId) {
        self.stats.skeleton_inst += 1;

        let inst = self.dfg.insts[&inst_id].clone();

        // CondBranch with a known-constant condition becomes an
        // unconditional Branch.
        if inst.opcode == Opcode::CondBranch {
            if let Some(BranchInfo::Conditional(then_block, then_count, else_block, else_count)) =
                inst.branch_info.clone()
            {
                if let Some(cond_val) = inst.args.first().copied() {
                    let const_val = self.resolve_to_const(cond_val).or_else(|| {
                        // Also accept a singleton range as a constant.
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
                        for &result in self.dfg.inst_results(inst_id) {
                            self.value_to_opt_value.insert(result, result);
                            self.available_block.insert(result, block);
                        }
                        return;
                    }
                }
            }
        }

        // GVN idempotent skeleton ops.
        if inst.opcode.is_mergeable() {
            let key = (inst.ty, inst.clone());

            match self.gvn_map.entry(key) {
                ScopedEntry::Occupied(entry) => {
                    if let Some(existing_value) = entry.get() {
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
                            }
                        }
                    }
                }
                ScopedEntry::Vacant(entry) => {
                    let result = self.dfg.first_result(inst_id);
                    self.value_to_opt_value.insert(result, result);
                    self.available_block.insert(result, block);
                    entry.insert(Some(result));
                }
            }
        } else {
            // Non-mergeable: identity-map the results.
            for &result in self.dfg.inst_results(inst_id) {
                self.value_to_opt_value.insert(result, result);
                self.available_block.insert(result, block);
            }
        }
    }

    /// Block where a pure inst becomes available — the deepest
    /// (furthest-from-entry) block where every arg is available.
    fn get_available_block(&self, inst_id: InstId) -> BlockId {
        let inst = &self.dfg.insts[&inst_id];

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
                // No args: available at entry.
                *self.gvn_map_blocks.first().unwrap()
            })
    }

    /// Block where a union becomes available — whichever side dominates.
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
