//! Elaboration: pull the best version of each value out of the egraph and
//! put it back into the CFG layout.

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

    /// value -> (best_cost, best_defining_inst)
    best_value_cache: HashMap<ValueId, (Cost, Option<InstId>)>,

    /// (value, block) -> elaborated value in that block
    elaborated_cache: HashMap<(ValueId, BlockId), ValueId>,

    /// Range assumptions active at the current extraction site.
    pub range_assumptions: Option<&'a mut RangeAssumptions>,

    /// Range facts per CFG edge, applied when entering each block.
    block_entry_facts: &'a FxHashMap<(BlockId, BlockId), Vec<(ValueId, crate::range::Range)>>,

    cfg_preds: &'a FxHashMap<BlockId, Vec<BlockId>>,

    /// Tracks which `best_value_cache` entries were touched at each domtree
    /// level so we can roll them back on Pop.
    cache_scope_stack: Vec<HashSet<ValueId>>,

    /// Blocks visited during the domtree walk. This is to remove
    /// unreachable blocks later
    pub visited_blocks: HashSet<BlockId>,

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
            visited_blocks: HashSet::new(),
            stats,
        }
    }

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
            visited_blocks: HashSet::new(),
            stats,
        }
    }

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

        // Seed the cost cache up front; Pop invalidations get re-filled lazily.
        self.compute_best_costs();

        while let Some(entry) = block_stack.pop() {
            match entry {
                StackEntry::Visit(block) => {
                    block_stack.push(StackEntry::Pop);
                    self.visited_blocks.insert(block);

                    if let Some(ra) = self.range_assumptions.as_deref_mut() {
                        ra.push_scope();

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

                    self.cache_scope_stack.push(HashSet::new());

                    self.elaborate_block(block, layout);

                    let dead_arm = self.fold_terminator(block, layout);

                    for &child in self.domtree.children(block).iter().rev() {
                        if Some(child) == dead_arm {
                            continue;
                        }
                        block_stack.push(StackEntry::Visit(child));
                    }
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

    fn compute_best_costs(&mut self) {
        let values: Vec<_> = self.dfg.value_defs.keys().copied().collect();

        for value in values {
            self.compute_best_cost(value);
        }
    }

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

    fn fold_terminator(&mut self, block: BlockId, layout: &Layout) -> Option<BlockId> {
        let term_id = layout
            .block_data
            .get(&block)
            .and_then(|b| b.insts.last().copied())?;
        let inst = self.dfg.insts.get(&term_id)?.clone();

        let (then_b, tc, else_b, ec) = match inst.branch_info {
            Some(BranchInfo::Conditional(t, tc, e, ec)) => (t, tc, e, ec),
            _ => return None,
        };

        if inst.args.is_empty() {
            return None;
        }
        let cond = inst.args[0];
        let elaborated_cond = self
            .elaborated_cache
            .get(&(cond, block))
            .copied()
            .unwrap_or(cond);

        if let Some(c) = self.dfg.value_imm(elaborated_cond) {
            let (target, count, offset, dead) = if c != 0 {
                (then_b, tc, 1usize, else_b)
            } else {
                (else_b, ec, 1 + tc, then_b)
            };
            let new_args: Vec<ValueId> = inst.args[offset..offset + count].to_vec();
            let new_inst = Instruction::with_branch(
                Opcode::Branch,
                new_args,
                inst.ty,
                BranchInfo::Jump(target),
            );
            self.dfg.insts.insert(term_id, new_inst);
            return if dead != target { Some(dead) } else { None };
        }

        if then_b == else_b && tc == ec {
            let then_args: Vec<ValueId> = (0..tc).map(|i| inst.args[1 + i]).collect();
            let else_args: Vec<ValueId> = (0..ec).map(|i| inst.args[1 + tc + i]).collect();
            if then_args == else_args {
                let new_inst = Instruction::with_branch(
                    Opcode::Branch,
                    then_args,
                    inst.ty,
                    BranchInfo::Jump(then_b),
                );
                self.dfg.insts.insert(term_id, new_inst);
            }
        }
        None
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
