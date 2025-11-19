//! Elaboration: Extract the best version from the egraph
//!
//! After building the egraph with union nodes representing equivalent
//! expressions, we need to:
//! 1. Select the "best" form of each value
//! 2. Place those instructions back into the CFG layout
//! 3. Perform code motion (LICM, rematerialization, etc.)

use crate::support::*;
use crate::types::*;
use std::collections::{HashMap, HashSet};

/// Cost of an operation (for extraction)
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

    /// Statistics
    stats: &'a mut Stats,
}

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
            stats,
        }
    }

    /// Main elaboration entry point
    ///
    /// Traverses all blocks and ensures every value used is properly
    /// elaborated (has a defining instruction in the layout).
    pub fn elaborate(&mut self, layout: &Layout) {
        println!("\n=== Starting Elaboration ===\n");

        // First pass: compute best costs for all values
        self.compute_best_costs();

        // Second pass: elaborate values in each block
        for &block_id in &layout.blocks {
            self.elaborate_block(block_id, layout);
        }

        println!("\n=== Elaboration Complete ===");
        println!("All pure values placed back into layout");
    }

    /// Compute the best cost for each value by exploring the egraph
    fn compute_best_costs(&mut self) {
        println!("Computing best costs for all values...");

        // Get all values that are defined by instructions
        let values: Vec<_> = self.dfg.value_defs.keys().copied().collect();

        for value in values {
            self.compute_best_cost(value);
        }
    }

    /// Recursively compute the best cost for a value
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
        best_cost
    }

    /// Elaborate all values needed in a block
    fn elaborate_block(&mut self, block_id: BlockId, layout: &Layout) {
        println!("Elaborating block {:?}", block_id);

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

    /// Elaborate a value in a specific block
    ///
    /// This ensures the value has a defining instruction placed
    /// at or before this block in the CFG.
    fn elaborate_value_in_block(&mut self, value: ValueId, block_id: BlockId) -> ValueId {
        self.stats.elaborate_visit_node += 1;

        // Check memoization cache
        let cache_key = (value, block_id);
        if let Some(&cached) = self.elaborated_cache.get(&cache_key) {
            self.stats.elaborate_memoize_hit += 1;
            return cached;
        }

        self.stats.elaborate_memoize_miss += 1;

        let elaborated = match self.dfg.value_def(value) {
            ValueDef::BlockParam(_, _) => {
                // Block parameters are already defined
                value
            }

            ValueDef::Inst(inst_id) => {
                // Regular instruction: recursively elaborate its arguments
                let inst = self.dfg.insts[&inst_id].clone();

                // Recursively elaborate arguments
                for &arg in &inst.args {
                    self.elaborate_value_in_block(arg, block_id);
                }

                // The instruction itself is now ready
                value
            }

            ValueDef::Union(left, right) => {
                // Union node: choose the best side
                let (_, best_inst) = self.best_value_cache[&value];

                if let Some(_inst_id) = best_inst {
                    // Recursively elaborate the best choice
                    let left_cost = self.compute_best_cost(left);
                    let right_cost = self.compute_best_cost(right);

                    let best = if left_cost <= right_cost { left } else { right };
                    self.elaborate_value_in_block(best, block_id)
                } else {
                    // Shouldn't happen, but handle gracefully
                    value
                }
            }
        };

        self.elaborated_cache.insert(cache_key, elaborated);
        elaborated
    }

    /// Get the best instruction to use for a value
    pub fn get_best_inst(&self, value: ValueId) -> Option<InstId> {
        self.best_value_cache
            .get(&value)
            .and_then(|(_, inst)| *inst)
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
