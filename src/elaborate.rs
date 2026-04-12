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

pub trait CostModel {
    fn cost_of_opcode(&self, opcode: Opcode) -> Cost;
}

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
    pub fn elaborate(&mut self, layout: &Layout) {
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

                    if let Some(ra) = self.range_assumptions.as_deref_mut() {
                        ra.push_scope();

                        // Apply block entry facts.
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

                    // Push a new cache-scope.
                    self.cache_scope_stack.push(HashSet::new());

                    // Elaborate block instructions
                    self.elaborate_block(block, layout);
                }

                StackEntry::Pop => {
                    if let Some(ra) = self.range_assumptions.as_deref_mut() {
                        ra.pop_scope();
                    }

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
        let values: Vec<_> = self.dfg.value_defs.keys().copied().collect();

        for value in values {
            self.compute_best_cost(value);
        }
    }

    /// Recursively compute the best cost for a value.
    fn compute_best_cost(&mut self, value: ValueId) -> Cost {
        if let Some(&(cost, _)) = self.best_value_cache.get(&value) {
            return cost;
        }

        let (best_cost, best_inst) = match self.dfg.value_def(value) {
            ValueDef::Inst(inst_id) => {
                let inst = &self.dfg.insts[&inst_id];
                let op_cost = self.cost_model.cost_of_opcode(inst.opcode);

                let mut total_cost = op_cost;
                let args = inst.args.clone();
                for arg in args {
                    let arg_cost = self.compute_best_cost(arg);
                    total_cost = total_cost.add(arg_cost);
                }

                (total_cost, Some(inst_id))
            }

            ValueDef::Union(left, right) => {
                let left_cost = self.compute_best_cost(left);
                let right_cost = self.compute_best_cost(right);

                if left_cost <= right_cost {
                    self.best_value_cache.get(&left).copied().unwrap()
                } else {
                    self.best_value_cache.get(&right).copied().unwrap()
                }
            }

            ValueDef::BlockParam(_, _) => (Cost::ZERO, None),
        };

        self.best_value_cache.insert(value, (best_cost, best_inst));

        self.consider_conditional_unions(value);

        self.best_value_cache[&value].0
    }

    fn consider_conditional_unions(&mut self, value: ValueId) {
        let ra = match self.range_assumptions.as_deref() {
            Some(ra) => ra as *const RangeAssumptions,
            None => return,
        };
        let ra = unsafe { &*ra };

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
                if let Some(scope) = self.cache_scope_stack.last_mut() {
                    scope.insert(value);
                }
            }
        }
    }

    fn elaborate_block(&mut self, block_id: BlockId, layout: &Layout) {
        let block = &layout.block_data[&block_id];

        for &inst_id in &block.insts {
            self.elaborate_inst_args(inst_id, block_id);
        }
    }

    fn elaborate_inst_args(&mut self, inst_id: InstId, block_id: BlockId) {
        let inst = self.dfg.insts[&inst_id].clone();

        for &arg in &inst.args {
            self.elaborate_value_in_block(arg, block_id);
        }
    }

    fn elaborate_value_in_block(&mut self, value: ValueId, block_id: BlockId) -> ValueId {
        self.stats.elaborate_visit_node += 1;

        let cache_key = (value, block_id);
        if let Some(&cached) = self.elaborated_cache.get(&cache_key) {
            self.stats.elaborate_memoize_hit += 1;
            return cached;
        }

        self.stats.elaborate_memoize_miss += 1;

        self.elaborated_cache.insert(cache_key, value);

        let elaborated = match self.dfg.value_def(value) {
            ValueDef::BlockParam(_, _) => value,

            ValueDef::Inst(inst_id) => {
                let inst = self.dfg.insts[&inst_id].clone();

                for &arg in &inst.args {
                    self.elaborate_value_in_block(arg, block_id);
                }

                self.best_conditional_alternative(value, block_id)
                    .unwrap_or(value)
            }

            ValueDef::Union(left, right) => {
                let left_cost = self.compute_best_cost(left);
                let right_cost = self.compute_best_cost(right);

                let best = if left_cost <= right_cost { left } else { right };
                self.elaborate_value_in_block(best, block_id)
            }
        };

        self.elaborated_cache.insert(cache_key, elaborated);
        elaborated
    }

    fn best_conditional_alternative(
        &mut self,
        value: ValueId,
        block_id: BlockId,
    ) -> Option<ValueId> {
        let ra = match self.range_assumptions.as_deref() {
            Some(ra) => ra as *const RangeAssumptions,
            None => return None,
        };
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

    pub fn get_best_inst(&self, value: ValueId) -> Option<InstId> {
        self.best_value_cache
            .get(&value)
            .and_then(|(_, inst)| *inst)
    }

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
