//! Main egraph pass implementation
//!
//! This pass does three main things:
//! 1. Removes pure operations from the layout (CFG)
//! 2. Builds an egraph with GVN and applies rewrite rules
//! 3. Elaborates (extracts) the best version back into the CFG

use crate::pattern::*;
use crate::range::{learn_from_comparison, RangeAssumptions};
use crate::rewrite_integration::*;
use crate::support::*;
use crate::types::*;
use std::collections::{HashMap, HashSet};

const _MATCHES_LIMIT: usize = 5;
const _ECLASS_ENODE_LIMIT: usize = 5;
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
        }
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
        println!("\n=== Starting Egraph Pass ===\n");

        // Print initial state
        println!("Initial IR (before optimization):");
        println!("{}", "─".repeat(60));
        let (func_name, sig_params, sig_return) = self.get_function_signature();
        println!(
            "{}",
            self.layout
                .display(&self.dfg, &func_name, &sig_params, sig_return)
        );

        // Phase 1: Remove pure instructions and build egraph
        self.remove_pure_and_optimize();

        println!("\n=== Egraph Built ===");
        println!("Pure instructions removed from layout");
        println!("Union nodes created for equivalent values\n");

        // Print intermediate state showing unions
        println!("After building egraph (with union nodes):");
        println!("{}", "─".repeat(60));
        self.print_egraph_state();

        let canonical_map = self.extract_best_values();
        self.rebuild_layout(&canonical_map);

        // TODO: Implement this
        // self.verify_egraph();

        // Print final statistics
        self.stats.print_summary();
        self.rewrite_engine.print_stats();
    }

    /// Extract the best (cheapest) representative from each equivalence class
    /// and update all instructions to use those representatives
    fn extract_best_values(&mut self) -> HashMap<ValueId, ValueId> {
        println!("Finding canonical representatives for all values...\n");

        // Build a map from each value to its canonical representative
        let mut canonical_map: HashMap<ValueId, ValueId> = HashMap::new();

        // Get all values
        let all_values: Vec<ValueId> = self.dfg.value_defs.keys().copied().collect();

        for value in all_values {
            let canonical = self.find_canonical(value, &mut canonical_map);
            canonical_map.insert(value, canonical);
        }

        // Update all instructions to use canonical representatives
        let all_insts: Vec<InstId> = self.dfg.insts.keys().copied().collect();
        let mut updated_count = 0;

        for inst_id in all_insts {
            let mut inst = self.dfg.insts[&inst_id].clone();
            let mut changed = false;

            // Update each argument to use its canonical representative
            for arg in &mut inst.args {
                if let Some(&canonical) = canonical_map.get(arg) {
                    if canonical != *arg {
                        *arg = canonical;
                        changed = true;
                    }
                }
            }

            if changed {
                self.dfg.insts.insert(inst_id, inst);
                updated_count += 1;
            }
        }

        println!(
            "\nExtraction complete: updated {} instructions\n",
            updated_count
        );

        canonical_map
    }

    /// Rebuild the layout to only include necessary instructions
    fn rebuild_layout(&mut self, _canonical_map: &HashMap<ValueId, ValueId>) {
        println!("Rebuilding layout with canonical values...");

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
            let original_count = block.insts.len();
            block.insts = new_insts;
            let new_count = block.insts.len();

            println!(
                "  Block {}: {} insts -> {} insts",
                block_id.0, original_count, new_count
            );
        }

        println!();
    }

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

    /// Find the canonical (simplest/cheapest) representative of a value
    /// by following union nodes and choosing the simplest form
    fn find_canonical(&self, value: ValueId, cache: &mut HashMap<ValueId, ValueId>) -> ValueId {
        // Check cache first
        if let Some(&cached) = cache.get(&value) {
            return cached;
        }

        // First apply GVN mapping if this value was merged
        let gvn_value = self.gvn_mapping.get(&value).copied().unwrap_or(value);

        // Collect all values that are equivalent to this one through unions
        let mut equiv_class = vec![gvn_value];
        let mut visited = HashSet::new();
        visited.insert(gvn_value);

        // BFS to find all equivalent values
        let mut queue = vec![gvn_value];
        while let Some(current) = queue.pop() {
            // Check if any union nodes reference this value
            for (&other_value, def) in &self.dfg.value_defs {
                if visited.contains(&other_value) {
                    continue;
                }

                if let ValueDef::Union(left, right) = def {
                    // If this union mentions current, add both sides to the equiv class
                    if *left == current || *right == current {
                        visited.insert(other_value);
                        visited.insert(*left);
                        visited.insert(*right);
                        equiv_class.push(other_value);
                        equiv_class.push(*left);
                        equiv_class.push(*right);
                        queue.push(other_value);
                        queue.push(*left);
                        queue.push(*right);
                    }
                }
            }
        }

        // Find the simplest value in the equivalence class
        let mut canonical = value;
        for &v in &equiv_class {
            if self.is_simpler_than(v, canonical) {
                canonical = v;
            }
        }

        cache.insert(value, canonical);
        canonical
    }

    /// Check if v1 is simpler than v2
    fn is_simpler_than(&self, v1: ValueId, v2: ValueId) -> bool {
        let def1 = self.dfg.value_def(v1);
        let def2 = self.dfg.value_def(v2);

        match (def1, def2) {
            // Block parameters are simplest
            (ValueDef::BlockParam(_, _), ValueDef::BlockParam(_, _)) => false,
            (ValueDef::BlockParam(_, _), _) => true,
            (_, ValueDef::BlockParam(_, _)) => false,

            // Union nodes are not canonical - skip them
            (ValueDef::Union(_, _), _) => false,
            (_, ValueDef::Union(_, _)) => true,

            // Compare instructions
            (ValueDef::Inst(inst1), ValueDef::Inst(inst2)) => {
                let i1 = &self.dfg.insts[&inst1];
                let i2 = &self.dfg.insts[&inst2];

                // Constants are simpler than non-constants
                if i1.opcode == Opcode::Const && i2.opcode != Opcode::Const {
                    return true;
                }
                if i2.opcode == Opcode::Const && i1.opcode != Opcode::Const {
                    return false;
                }

                // Fewer arguments is simpler
                if i1.args.len() < i2.args.len() {
                    return true;
                }
                if i2.args.len() < i1.args.len() {
                    return false;
                }

                // Cheaper opcode is simpler
                use crate::elaborate::{CostModel, DefaultCostModel};
                let cost_model = DefaultCostModel;
                let cost1 = cost_model.cost_of_opcode(i1.opcode);
                let cost2 = cost_model.cost_of_opcode(i2.opcode);

                cost1 < cost2
            }
        }
    }

    // TODO: Implement this
    // fn verify_egraph(&self) {
    //     // We could add other verification checks here, such as:
    //     // - Checking for circular references in union nodes
    //     // - Verifying that all referenced values exist
    //     // - Checking type consistency
    // }

    /// Extract function signature from entry block
    fn get_function_signature(&self) -> (String, Vec<Type>, Option<Type>) {
        let func_name = "test".to_string();

        // Get parameter types from entry block
        let sig_params: Vec<Type> = if let Some(entry_block) = self.layout.entry_block() {
            if let Some(block_data) = self.layout.block_data.get(&entry_block) {
                block_data
                    .params
                    .iter()
                    .map(|&param| self.dfg.value_type(param))
                    .collect()
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };

        // Try to infer return type from return instructions
        let sig_return = None; // Could be enhanced to find return instructions

        (func_name, sig_params, sig_return)
    }

    /// Print the current state of the egraph including union nodes
    fn print_egraph_state(&self) {
        // Show all value definitions including unions
        println!("Value definitions:");
        let mut values: Vec<_> = self.dfg.value_defs.keys().copied().collect();
        values.sort();

        for value in values {
            let def_str = self.dfg.display_value_def(value);
            match self.dfg.value_def(value) {
                ValueDef::Union(_, _) => {
                    println!("  {} ← UNION NODE", def_str);
                }
                ValueDef::BlockParam(_, _) => {
                    println!("  {} ← BLOCK PARAM", def_str);
                }
                ValueDef::Inst(_) => {
                    println!("  {}", def_str);
                }
            }
        }
        println!();

        // Show the layout (skeleton)
        println!("Remaining skeleton (control flow):");
        let (func_name, sig_params, sig_return) = self.get_function_signature();
        println!(
            "{}",
            self.layout
                .display(&self.dfg, &func_name, &sig_params, sig_return)
        );
    }

    /// Add a custom rewrite rule to the engine
    pub fn add_rewrite_rule(&mut self, rule: Rewrite) {
        self.rewrite_engine.add_rule(rule);
    }

    /// Remove pure operations from layout and build egraph
    ///
    /// This walks through all blocks in dominator-tree preorder:
    /// - For pure instructions: remove from layout, add to egraph with GVN
    /// - For skeleton instructions: keep in layout, apply GVN if idempotent
    /// - Apply rewrite rules eagerly as nodes are created
    fn remove_pure_and_optimize(&mut self) {
        // Map from original value to its optimized version
        let mut value_to_opt_value: HashMap<ValueId, ValueId> = HashMap::new();

        let mut gvn_map: ScopedHashMap<(Type, Instruction), Option<ValueId>> =
            ScopedHashMap::with_capacity(100);

        let mut available_block: HashMap<ValueId, BlockId> = HashMap::new();

        let mut eclass_size: HashMap<ValueId, u8> = HashMap::new();

        let mut gvn_map_blocks: Vec<BlockId> = Vec::new();

        let remat_values: HashSet<ValueId> = HashSet::new();

        let mut branch_conditions: HashMap<BlockId, Vec<(ValueId, Option<i64>, Opcode, bool)>> =
            HashMap::new();

        // Pre-compute branch conditions for each block by scanning all blocks
        for &block_id in &self.layout.blocks {
            let block = &self.layout.block_data[&block_id];
            for &inst_id in &block.insts {
                let inst = &self.dfg.insts[&inst_id];
                if inst.opcode == Opcode::CondBranch {
                    if let Some(BranchInfo::Conditional(then_block, _, else_block, _)) =
                        &inst.branch_info
                    {
                        // Get the condition value (first arg)
                        if !inst.args.is_empty() {
                            let cond_value = inst.args[0];

                            // Try to find if the condition is a comparison
                            if let ValueDef::Inst(cond_inst_id) = self.dfg.value_def(cond_value) {
                                let cond_inst = &self.dfg.insts[&cond_inst_id];
                                if cond_inst.opcode.is_comparison() {
                                    // Get the LHS and see if RHS is a constant
                                    let lhs = cond_inst.args.get(0).copied();
                                    let rhs_const = cond_inst.args.get(1).and_then(|&rhs| {
                                        if let ValueDef::Inst(rhs_inst) = self.dfg.value_def(rhs) {
                                            let rhs_inst = &self.dfg.insts[&rhs_inst];
                                            if rhs_inst.opcode == Opcode::Const {
                                                return rhs_inst.immediate;
                                            }
                                        }
                                        None
                                    });

                                    if let Some(lhs) = lhs {
                                        // For then block, condition is true
                                        branch_conditions.entry(*then_block).or_default().push((
                                            lhs,
                                            rhs_const,
                                            cond_inst.opcode,
                                            true,
                                        ));

                                        // For else block, condition is false
                                        branch_conditions.entry(*else_block).or_default().push((
                                            lhs,
                                            rhs_const,
                                            cond_inst.opcode,
                                            false,
                                        ));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        let root = self.layout.entry_block().unwrap();

        enum StackEntry {
            Visit(BlockId),
            Pop,
        }

        let mut block_stack = vec![StackEntry::Visit(root)];

        while let Some(entry) = block_stack.pop() {
            match entry {
                StackEntry::Visit(block) => {
                    println!("Processing block {:?}", block);

                    block_stack.push(StackEntry::Pop);
                    for &child in self.domtree.children(block) {
                        block_stack.push(StackEntry::Visit(child));
                    }

                    gvn_map.increment_depth();
                    gvn_map_blocks.push(block);

                    // Push scope for range assumptions
                    self.range_assumptions.push_scope();

                    // Learn range facts from branch conditions that lead to this block
                    if let Some(conditions) = branch_conditions.get(&block) {
                        for &(lhs, rhs_const, opcode, is_true_branch) in conditions {
                            let facts =
                                learn_from_comparison(opcode, lhs, rhs_const, is_true_branch);
                            for (value, range) in facts {
                                println!("    Learning range fact: {:?} in {}", value, range);
                                self.range_assumptions.assume_range(value, range);
                            }
                        }
                    }

                    let block_data = self.layout.block_data[&block].clone();
                    for &param in &block_data.params {
                        value_to_opt_value.insert(param, param);
                        available_block.insert(param, block);
                    }

                    let inst_ids: Vec<_> = block_data.insts.clone();

                    for inst_id in inst_ids {
                        let inst = self.dfg.insts[&inst_id].clone();

                        println!("  Processing inst {:?}: {:?}", inst_id, inst.opcode);

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
                            eclass_size: &mut eclass_size,
                            remat_values: &remat_values,
                            stats: &mut self.stats,
                            domtree: &self.domtree,
                            rewrite_depth: 0,
                            subsume_values: HashSet::new(),
                            rewrite_engine: &mut self.rewrite_engine,
                            range_assumptions: &mut self.range_assumptions,
                        };

                        if rewritten_inst.opcode.is_pure() {
                            ctx.insert_pure_enode(inst_id);
                            println!("    -> Pure inst, added to egraph");
                        } else {
                            ctx.optimize_skeleton_inst(inst_id, block);
                            println!("    -> Skeleton inst, kept in layout");
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
    }
}

/// Context for optimization
struct OptimizeCtx<'a> {
    dfg: &'a mut DataFlowGraph,
    value_to_opt_value: &'a mut HashMap<ValueId, ValueId>,
    gvn_map: &'a mut ScopedHashMap<(Type, Instruction), Option<ValueId>>,
    gvn_map_blocks: &'a Vec<BlockId>,
    available_block: &'a mut HashMap<ValueId, BlockId>,
    eclass_size: &'a mut HashMap<ValueId, u8>,
    remat_values: &'a HashSet<ValueId>,
    stats: &'a mut Stats,
    domtree: &'a DominatorTree,
    rewrite_depth: usize,
    subsume_values: HashSet<ValueId>,
    rewrite_engine: &'a mut RewriteEngine,
    range_assumptions: &'a mut RangeAssumptions,
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

                    println!("      GVN hit: merged with {:?}", existing_value);
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

                println!("      New instruction: {:?}", result);

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

        let rewrites = self.rewrite_engine.apply_rewrites(self.dfg, value);

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

            // Create union between original and rewritten
            let new_union = self.dfg.make_union(union_value, opt_value);
            self.stats.union += 1;

            // Merge availability: use the block that dominates both
            let avail = self.merge_availability(union_value, opt_value);
            self.available_block.insert(new_union, avail);

            union_value = new_union;

            println!(
                "        Created union {:?} = {:?} ∪ {:?}",
                new_union, union_value, opt_value
            );
        }

        self.rewrite_depth -= 1;
        union_value
    }

    /// Optimize a skeleton instruction
    ///
    /// These instructions stay in the layout but can still benefit from:
    /// - GVN (if idempotent)
    /// - Alias analysis (for loads/stores)
    fn optimize_skeleton_inst(&mut self, inst_id: InstId, block: BlockId) {
        self.stats.skeleton_inst += 1;

        let inst = self.dfg.insts[&inst_id].clone();

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

                                println!(
                                    "      Skeleton GVN hit: merged with {:?}",
                                    existing_value
                                );
                                return;
                            } else {
                                println!("      Cannot merge: different conditions");
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_gvn() {
        // Create a simple function: x = a + b; y = a + b
        let mut dfg = DataFlowGraph::new();

        let block = BlockId(0);
        let a = dfg.make_block_param(block, 0, Type::I32);
        let b = dfg.make_block_param(block, 1, Type::I32);

        // First add: x = a + b
        let inst1 = Instruction::new(Opcode::Add, vec![a, b], Type::I32);
        let inst1_id = dfg.make_inst(inst1);
        let _x = dfg.make_inst_result(inst1_id, Type::I32);

        // Second add: y = a + b (should be deduplicated)
        let inst2 = Instruction::new(Opcode::Add, vec![a, b], Type::I32);
        let inst2_id = dfg.make_inst(inst2);
        let _y = dfg.make_inst_result(inst2_id, Type::I32);

        let mut layout = Layout::new();
        let mut block_data = Block::new(block);
        block_data.params = vec![a, b];
        block_data.insts = vec![inst1_id, inst2_id];
        layout.add_block(block_data);

        let domtree = DominatorTree::from_linear_blocks(&[block]);

        let mut pass = EgraphPass::new(dfg, layout, domtree);
        pass.run();

        // After GVN, both adds should be deduplicated
        assert!(pass.stats.pure_inst_deduped > 0);
        println!("\n✓ GVN test passed");
    }

    #[test]
    fn test_pattern_based_rewrites() {
        let mut dfg = DataFlowGraph::new();
        let block = BlockId(0);
        let x = dfg.make_block_param(block, 0, Type::I32);

        // Create: zero = const 0
        let zero_inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, 0);
        let zero_id = dfg.make_inst(zero_inst);
        let zero = dfg.make_inst_result(zero_id, Type::I32);

        // Create: y = x + 0
        let add_inst = Instruction::new(Opcode::Add, vec![x, zero], Type::I32);
        let add_id = dfg.make_inst(add_inst);
        let _y = dfg.make_inst_result(add_id, Type::I32);

        let mut layout = Layout::new();
        let mut block_data = Block::new(block);
        block_data.params = vec![x];
        block_data.insts = vec![zero_id, add_id];
        layout.add_block(block_data);

        let domtree = DominatorTree::from_linear_blocks(&[block]);

        let mut pass = EgraphPass::new(dfg, layout, domtree);
        pass.run();

        // Should have applied pattern-based rewrites
        assert!(pass.stats.rewrite_rule_results > 0 || pass.stats.pure_inst_deduped > 0);
        println!("\n✓ Pattern-based rewrite test passed");
    }
}
