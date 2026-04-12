//! Elaboration: Extract the best version from the egraph
//!
//! After building the egraph with union nodes representing equivalent
//! expressions, we need to:
//! 1. Select the "best" form of each value
//! 2. Place those instructions back into the CFG layout

use crate::range::RangeAssumptions;
use crate::support::*;
use crate::types::*;
use rustc_hash::FxHashMap;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Cost(pub u32);

impl Cost {
    pub const ZERO: Cost = Cost(0);
    pub const INFINITY: Cost = Cost(u32::MAX);

    pub fn add(self, other: Cost) -> Cost {
        if self == Cost::INFINITY || other == Cost::INFINITY {
            Cost::INFINITY
        } else {
            Cost(self.0.saturating_add(other.0))
        }
    }
}

/// Cost model for operations
pub trait CostModel {
    fn cost_of_opcode(&self, opcode: Opcode) -> Cost;
}

/// Default cost model: assigns costs based on operation type
pub struct DefaultCostModel;

impl CostModel for DefaultCostModel {
    fn cost_of_opcode(&self, opcode: Opcode) -> Cost {
        match opcode {
            Opcode::Const => Cost::ZERO,
            Opcode::Add | Opcode::Sub => Cost(1),
            Opcode::AddImm => Cost(1),
            Opcode::Mul => Cost(2),
            Opcode::MulImm => Cost(2),
            Opcode::Irem => Cost(5),
            Opcode::Div => Cost(5),
            Opcode::And | Opcode::Or | Opcode::Xor => Cost(1),
            Opcode::Shl | Opcode::Ushr | Opcode::Sshr => Cost(1),
            Opcode::ShlImm => Cost(1),
            Opcode::Bnot | Opcode::Ineg => Cost(1),
            Opcode::Eq | Opcode::Ne => Cost(1),
            Opcode::Slt | Opcode::Sle | Opcode::Sgt | Opcode::Sge => Cost(1),
            Opcode::Ult | Opcode::Ule | Opcode::Ugt | Opcode::Uge => Cost(1),
            Opcode::Uextend | Opcode::Sextend => Cost(1),
            Opcode::Load => Cost(10),
            Opcode::Store => Cost(10),
            Opcode::Call => Cost(20),
            Opcode::Branch | Opcode::CondBranch | Opcode::Return => Cost(5),
            Opcode::Trap => Cost(100),
            Opcode::Sdiv | Opcode::Udiv | Opcode::Urem | Opcode::Srem => Cost(5),
            Opcode::Iabs => Cost(1),
            Opcode::Select => Cost(1),
            Opcode::Ireduce => Cost(1),
            Opcode::Clz | Opcode::Ctz => Cost(1),
            Opcode::Rotl | Opcode::Rotr => Cost(1),
        }
    }
}

/// Elaborator: extracts best values from egraph and places them in CFG
pub struct Elaborator<'a> {
    dfg: &'a mut DataFlowGraph,
    domtree: &'a DominatorTree,
    cost_model: DefaultCostModel,

    /// Cache: value -> (best_cost, best_defining_inst)
    best_value_cache: HashMap<ValueId, (Cost, Option<InstId>)>,

    /// Memoization: (value, block) -> elaborated value in that block
    elaborated_cache: HashMap<(ValueId, BlockId), ValueId>,

    /// Mutable range assumptions active at the current extraction site.
    pub range_assumptions: Option<&'a mut RangeAssumptions>,

    /// Block entry facts per CFG edge, used to apply range assumptions
    /// when entering each block during the domtree walk.
    block_entry_facts: &'a FxHashMap<(BlockId, BlockId), Vec<(ValueId, crate::range::Range)>>,

    /// CFG predecessor map for looking up incoming edges per block.
    cfg_preds: &'a FxHashMap<BlockId, Vec<BlockId>>,

    /// Per-domtree-level sets of `ValueId`s whose `best_value_cache` entries
    /// were inserted or modified at that level.
    cache_scope_stack: Vec<HashSet<ValueId>>,

    stats: &'a mut Stats,
}

static EMPTY_ENTRY_FACTS: std::sync::LazyLock<
    FxHashMap<(BlockId, BlockId), Vec<(ValueId, crate::range::Range)>>,
> = std::sync::LazyLock::new(FxHashMap::default);
static EMPTY_CFG_PREDS: std::sync::LazyLock<FxHashMap<BlockId, Vec<BlockId>>> =
    std::sync::LazyLock::new(FxHashMap::default);

impl<'a> Elaborator<'a> {
    pub fn new(
        dfg: &'a mut DataFlowGraph,
        domtree: &'a DominatorTree,
        stats: &'a mut Stats,
    ) -> Self {
        Self {
            dfg,
            domtree,
            cost_model: DefaultCostModel,
            best_value_cache: HashMap::new(),
            elaborated_cache: HashMap::new(),
            range_assumptions: None,
            block_entry_facts: &EMPTY_ENTRY_FACTS,
            cfg_preds: &EMPTY_CFG_PREDS,
            cache_scope_stack: Vec::new(),
            stats,
        }
    }

    /// Create an elaborator with range assumptions for context-aware extraction.
    pub fn with_range_assumptions(
        dfg: &'a mut DataFlowGraph,
        domtree: &'a DominatorTree,
        stats: &'a mut Stats,
        range_assumptions: &'a mut RangeAssumptions,
        block_entry_facts: &'a FxHashMap<(BlockId, BlockId), Vec<(ValueId, crate::range::Range)>>,
        cfg_preds: &'a FxHashMap<BlockId, Vec<BlockId>>,
    ) -> Self {
        Self {
            dfg,
            domtree,
            cost_model: DefaultCostModel,
            best_value_cache: HashMap::new(),
            elaborated_cache: HashMap::new(),
            range_assumptions: Some(range_assumptions),
            block_entry_facts,
            cfg_preds,
            cache_scope_stack: Vec::new(),
            stats,
        }
    }

    /// Main elaboration entry point.
    ///
    /// Performs a domtree-preorder walk (Visit/Pop stack pattern) so that
    /// conditional unions are evaluated in the correct scoped context.
    pub fn elaborate(&mut self, layout: &Layout) {
        if self.range_assumptions.is_some() {
            self.elaborate_domtree(layout);
        } else {
            self.compute_best_costs();
            for &block_id in &layout.blocks {
                self.elaborate_block(block_id, layout);
            }
        }
    }

    /// Domtree-preorder elaboration walk.
    ///
    /// Mirrors the Visit/Pop stack pattern from `remove_pure_and_optimize`:
    /// - On `Visit(block)`: push a range assumption scope, apply entry facts,
    ///   recompute best costs under the new assumptions, then elaborate.
    /// - On `Pop`: pop the range assumption scope and invalidate cache entries
    ///   that were influenced by the now-departed conditional context.
    fn elaborate_domtree(&mut self, layout: &Layout) {
        use crate::range::Range;

        enum StackEntry {
            Visit(BlockId),
            Pop,
        }

        let root = match layout.entry_block() {
            Some(b) => b,
            None => return,
        };

        let mut block_stack = vec![StackEntry::Visit(root)];

        // Compute best costs once before traversal; subsequent cache misses
        // after Pop-driven invalidation are handled lazily by compute_best_cost.
        self.compute_best_costs();

        while let Some(entry) = block_stack.pop() {
            match entry {
                StackEntry::Visit(block) => {
                    block_stack.push(StackEntry::Pop);
                    // Push domtree children in reverse order for correct preorder
                    for &child in self.domtree.children(block).iter().rev() {
                        block_stack.push(StackEntry::Visit(child));
                    }

                    // --- Push range assumption scope ---
                    // Safety: we only access range_assumptions mutably here and
                    // the borrows in consider_conditional_unions / best_conditional_alternative
                    // use raw-pointer casts that do not alias with push/pop.
                    let ra = self.range_assumptions.as_deref_mut().unwrap();
                    ra.push_scope();

                    // Apply block entry facts (union across predecessors, same
                    // logic as remove_pure_and_optimize).
                    {
                        let preds = self.cfg_preds.get(&block).cloned().unwrap_or_default();
                        let num_edges = preds.len();
                        if num_edges > 0 {
                            let mut value_edge_count: HashMap<ValueId, usize> = HashMap::new();
                            let mut value_union: HashMap<ValueId, Range> = HashMap::new();
                            for pred in &preds {
                                if let Some(facts) = self.block_entry_facts.get(&(*pred, block)) {
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
                            let ra = self.range_assumptions.as_deref_mut().unwrap();
                            for (value, range) in &value_union {
                                if value_edge_count.get(value) == Some(&num_edges)
                                    && !range.is_unbounded()
                                {
                                    ra.assume_range(*value, *range);
                                }
                            }
                        }
                    }

                    // Push a new cache-scope level to track entries added
                    // under the current domtree context.
                    self.cache_scope_stack.push(HashSet::new());

                    // Elaborate block instructions
                    self.elaborate_block(block, layout);
                }

                StackEntry::Pop => {
                    // Pop range assumption scope
                    let ra = self.range_assumptions.as_deref_mut().unwrap();
                    ra.pop_scope();

                    // Invalidate cache entries that were added/modified at
                    // this domtree level — they may have been influenced by
                    // conditional unions that are no longer in scope.
                    if let Some(dirty) = self.cache_scope_stack.pop() {
                        for value in dirty {
                            self.best_value_cache.remove(&value);
                        }
                    }
                }
            }
        }
    }

    /// Compute the best cost for each value by exploring the egraph
    fn compute_best_costs(&mut self) {
        // Get all values that are defined by instructions
        let values: Vec<_> = self.dfg.value_defs.keys().copied().collect();

        for value in values {
            self.compute_best_cost(value);
        }
    }

    /// Recursively compute the best cost for a value.
    ///
    /// In addition to unconditional e-class members (via `ValueDef::Union`
    /// chains), this consults `dfg.conditional_unions` for any conditional
    /// equivalences whose `AssumptionSet` is entailed by the active
    /// `range_assumptions`, including those members as additional candidates.
    fn compute_best_cost(&mut self, value: ValueId) -> Cost {
        // Check cache
        if let Some(&(cost, _)) = self.best_value_cache.get(&value) {
            return cost;
        }

        let (best_cost, best_inst) = match self.dfg.value_def(value) {
            ValueDef::Inst(inst_id) => {
                // Regular instruction: cost = op_cost + sum(arg_costs)
                let inst = &self.dfg.insts[&inst_id];
                let op_cost = self.cost_model.cost_of_opcode(inst.opcode);

                let mut total_cost = op_cost;
                let args = inst.args.clone(); // Clone to avoid borrowing issues
                for arg in args {
                    let arg_cost = self.compute_best_cost(arg);
                    total_cost = total_cost.add(arg_cost);
                }

                (total_cost, Some(inst_id))
            }

            ValueDef::Union(left, right) => {
                // Union node: take minimum cost of either side
                let left_cost = self.compute_best_cost(left);
                let right_cost = self.compute_best_cost(right);

                if left_cost <= right_cost {
                    self.best_value_cache.get(&left).copied().unwrap()
                } else {
                    self.best_value_cache.get(&right).copied().unwrap()
                }
            }

            ValueDef::BlockParam(_, _) => {
                // Block parameters are free (already defined)
                (Cost::ZERO, None)
            }
        };

        self.best_value_cache.insert(value, (best_cost, best_inst));

        // Additionally, check satisfied conditional unions for cheaper alternatives.
        self.consider_conditional_unions(value);

        self.best_value_cache[&value].0
    }

    /// Check conditional unions that mention `value` and, if their assumptions
    /// are entailed by the active range context, include the other side as a
    /// candidate that may lower the best cost.
    fn consider_conditional_unions(&mut self, value: ValueId) {
        let ra = match self.range_assumptions.as_deref() {
            Some(ra) => ra as *const RangeAssumptions,
            None => return,
        };
        // SAFETY: The pointee (`RangeAssumptions`) is not mutated through
        // the raw pointer while this shared reference is live.  The cast
        // exists solely to satisfy the borrow checker — `self` is borrowed
        // mutably for `compute_best_cost`, but `range_assumptions` is only
        // read (not written) during the iteration below.
        let ra = unsafe { &*ra };

        // Collect candidates from conditional unions.
        let candidates: Vec<ValueId> = self
            .dfg
            .conditional_unions
            .iter()
            .filter_map(|cu| {
                if !cu.condition.is_entailed_by(ra) {
                    return None;
                }
                if cu.lhs == value {
                    Some(cu.rhs)
                } else if cu.rhs == value {
                    Some(cu.lhs)
                } else {
                    None
                }
            })
            .collect();

        for candidate in candidates {
            let cand_cost = self.compute_best_cost(candidate);
            let current = self.best_value_cache[&value];
            if cand_cost < current.0 {
                let cand_entry = self.best_value_cache[&candidate];
                self.best_value_cache.insert(value, cand_entry);
                // Track this value as dirty at the current domtree level
                if let Some(scope) = self.cache_scope_stack.last_mut() {
                    scope.insert(value);
                }
            }
        }
    }

    /// Elaborate all values needed in a block
    fn elaborate_block(&mut self, block_id: BlockId, layout: &Layout) {
        let block = &layout.block_data[&block_id];

        // Process each instruction in the block
        for &inst_id in &block.insts {
            self.elaborate_inst_args(inst_id, block_id);
        }
    }

    /// Ensure all arguments of an instruction are elaborated
    fn elaborate_inst_args(&mut self, inst_id: InstId, block_id: BlockId) {
        let inst = self.dfg.insts[&inst_id].clone();

        for &arg in &inst.args {
            self.elaborate_value_in_block(arg, block_id);
        }
    }

    /// Elaborate a value in a specific block.
    ///
    /// This ensures the value has a defining instruction placed at or before
    /// this block in the CFG. When range assumptions are active, satisfied
    /// conditional unions are considered as additional candidates.
    fn elaborate_value_in_block(&mut self, value: ValueId, block_id: BlockId) -> ValueId {
        self.stats.elaborate_visit_node += 1;

        // Check memoization cache
        let cache_key = (value, block_id);
        if let Some(&cached) = self.elaborated_cache.get(&cache_key) {
            self.stats.elaborate_memoize_hit += 1;
            return cached;
        }

        self.stats.elaborate_memoize_miss += 1;

        // Provisional entry breaks cycles when a union references this value
        // and we recursively elaborate back to it.
        self.elaborated_cache.insert(cache_key, value);

        let elaborated = match self.dfg.value_def(value) {
            ValueDef::BlockParam(_, _) => {
                // Block parameters are already defined
                value
            }

            ValueDef::Inst(inst_id) => {
                // Regular instruction: recursively elaborate its arguments
                let inst = self.dfg.insts[&inst_id].clone();

                for &arg in &inst.args {
                    self.elaborate_value_in_block(arg, block_id);
                }

                self.best_conditional_alternative(value, block_id)
                    .unwrap_or(value)
            }

            ValueDef::Union(left, right) => {
                // Union node: choose the best side
                let left_cost = self.compute_best_cost(left);
                let right_cost = self.compute_best_cost(right);

                let best = if left_cost <= right_cost { left } else { right };
                self.elaborate_value_in_block(best, block_id)
            }
        };

        self.elaborated_cache.insert(cache_key, elaborated);
        elaborated
    }

    /// If a satisfied conditional union offers a strictly cheaper alternative
    /// for `value`, return it. Otherwise return `None`.
    fn best_conditional_alternative(
        &mut self,
        value: ValueId,
        block_id: BlockId,
    ) -> Option<ValueId> {
        let ra = match self.range_assumptions.as_deref() {
            Some(ra) => ra as *const RangeAssumptions,
            None => return None,
        };
        // SAFETY: The pointee is not mutated through the raw pointer while
        // this shared reference is live.  We only read `range_assumptions`
        // here; mutation happens elsewhere via `&mut self`.
        let ra = unsafe { &*ra };

        let current_cost = self.compute_best_cost(value);

        let candidates: Vec<ValueId> = self
            .dfg
            .conditional_unions
            .iter()
            .filter_map(|cu| {
                if !cu.condition.is_entailed_by(ra) {
                    return None;
                }
                if cu.lhs == value {
                    Some(cu.rhs)
                } else if cu.rhs == value {
                    Some(cu.lhs)
                } else {
                    None
                }
            })
            .collect();

        let mut best: Option<(Cost, ValueId)> = None;
        for cand in candidates {
            let cost = self.compute_best_cost(cand);
            if cost < current_cost {
                if best.map_or(true, |(bc, _)| cost < bc) {
                    best = Some((cost, cand));
                }
            }
        }

        best.map(|(_, v)| self.elaborate_value_in_block(v, block_id))
    }

    /// Get the best instruction to use for a value
    pub fn get_best_inst(&self, value: ValueId) -> Option<InstId> {
        self.best_value_cache
            .get(&value)
            .and_then(|(_, inst)| *inst)
    }

    /// After elaboration, rewrite all instruction arguments to use their
    /// best-cost representatives.  For each instruction in each block, every
    /// argument is replaced with the value that `elaborate_value_in_block`
    /// selected for that block context, so conditional unions are respected.
    pub fn rewrite_args(&mut self, layout: &Layout) {
        for &block_id in &layout.blocks {
            let block = match layout.block_data.get(&block_id) {
                Some(b) => b,
                None => continue,
            };

            for &inst_id in &block.insts {
                let mut inst = self.dfg.insts[&inst_id].clone();
                let mut changed = false;

                for arg in &mut inst.args {
                    if let Some(&elaborated) = self.elaborated_cache.get(&(*arg, block_id)) {
                        if elaborated != *arg {
                            *arg = elaborated;
                            changed = true;
                        }
                    }
                }

                if changed {
                    self.dfg.insts.insert(inst_id, inst);
                }
            }
        }
    }
}

/// Simple LICM (Loop-Invariant Code Motion) analysis
///
/// In a full implementation, this would use loop analysis to hoist
/// loop-invariant computations out of loops.
pub struct SimpleLICM {
    /// Values that should be hoisted
    pub hoist_values: HashSet<ValueId>,
}

impl SimpleLICM {
    pub fn new() -> Self {
        Self {
            hoist_values: HashSet::new(),
        }
    }

    /// Analyze and mark values for hoisting
    pub fn analyze(&mut self, _dfg: &DataFlowGraph, _layout: &Layout) {
        // Simplified: in a real implementation, this would:
        // 1. Identify loops
        // 2. Find loop-invariant computations
        // 3. Mark them for hoisting

        // For now, this is a placeholder
    }

    /// Check if a value should be hoisted
    pub fn should_hoist(&self, value: ValueId) -> bool {
        self.hoist_values.contains(&value)
    }
}

/// Rematerialization analysis
///
/// Determines when it's better to recompute a value rather than
/// keep it live across a large region (reduces register pressure).
pub struct RematerializationAnalysis {
    pub remat_values: HashSet<ValueId>,
}

impl RematerializationAnalysis {
    pub fn new() -> Self {
        Self {
            remat_values: HashSet::new(),
        }
    }

    /// Analyze and mark values for rematerialization
    pub fn analyze(&mut self, dfg: &DataFlowGraph) {
        // Mark cheap values (like constants) for rematerialization
        for (&value, &def) in &dfg.value_defs {
            if let ValueDef::Inst(inst_id) = def {
                let inst = &dfg.insts[&inst_id];

                // Remat constants and other cheap operations
                if matches!(inst.opcode, Opcode::Const) {
                    self.remat_values.insert(value);
                }
            }
        }
    }

    /// Check if a value should be rematerialized
    pub fn should_remat(&self, value: ValueId) -> bool {
        self.remat_values.contains(&value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cost_computation() {
        let mut dfg = DataFlowGraph::new();

        // Create: x = const 5, y = x + x
        let const_inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, 5);
        let const_id = dfg.make_inst(const_inst);
        let x = dfg.make_inst_result(const_id, Type::I32);

        let add_inst = Instruction::new(Opcode::Add, vec![x, x], Type::I32);
        let add_id = dfg.make_inst(add_inst);
        let y = dfg.make_inst_result(add_id, Type::I32);

        let domtree = DominatorTree::new();
        let mut stats = Stats::default();

        let mut elaborator = Elaborator::new(&mut dfg, &domtree, &mut stats);
        elaborator.compute_best_costs();

        // x should have cost 0 (constant)
        let x_cost = elaborator.compute_best_cost(x);
        assert_eq!(x_cost, Cost::ZERO);

        // y should have cost 1 (add) + 0 (x) + 0 (x) = 1
        let y_cost = elaborator.compute_best_cost(y);
        assert_eq!(y_cost, Cost(1));

        println!("✓ Cost computation test passed");
    }

    #[test]
    fn test_union_cost_selection() {
        let mut dfg = DataFlowGraph::new();

        // Create: x (var)
        let block = BlockId(0);
        let x = dfg.make_block_param(block, 0, Type::I32);

        // Create: y = x * 2 (cost 2)
        let two = {
            let inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, 2);
            let id = dfg.make_inst(inst);
            dfg.make_inst_result(id, Type::I32)
        };
        let mul_inst = Instruction::new(Opcode::Mul, vec![x, two], Type::I32);
        let mul_id = dfg.make_inst(mul_inst);
        let y_mul = dfg.make_inst_result(mul_id, Type::I32);

        // Create: z = x + x (cost 1)
        let add_inst = Instruction::new(Opcode::Add, vec![x, x], Type::I32);
        let add_id = dfg.make_inst(add_inst);
        let z_add = dfg.make_inst_result(add_id, Type::I32);

        // Create union: y_mul ∪ z_add
        let union = dfg.make_union(y_mul, z_add);

        let domtree = DominatorTree::new();
        let mut stats = Stats::default();

        let mut elaborator = Elaborator::new(&mut dfg, &domtree, &mut stats);
        elaborator.compute_best_costs();

        // Union should pick the cheaper option (add, cost 1)
        let union_cost = elaborator.compute_best_cost(union);
        assert_eq!(union_cost, Cost(1));

        println!("✓ Union cost selection test passed");
    }
}
