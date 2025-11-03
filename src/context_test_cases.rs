//! Examples that missed optimizations due to context blindness

use crate::egraph_pass::*;
use crate::support::*;
use crate::types::*;

/// Example 1: Missed constant propagation through phi-nodes
///
/// Original IR:
///   block0(cond: i32):
///     br_if cond, block1, block2
///
///   block1:
///     v1 = const 10
///     jump block3(v1)
///
///   block2:
///     v2 = const 10
///     jump block3(v2)
///
///   block3(v3: i32):  // phi node merging v1 and v2
///     v4 = v3 + 5     // Could be optimized to const 15!
///     return v4
///
/// **Context-blind aegraph**: Sees v3 as unknown in block3, misses optimization
/// **Context-aware aegraph**: Knows v3 = 10 on all paths, optimizes to const 15
pub fn example_phi_constant_propagation() {
    println!("\n=== Example 1: Phi-Node Constants ===\n");

    let mut dfg = DataFlowGraph::new();
    let mut layout = Layout::new();

    let block0 = BlockId(0);
    let mut block0_data = Block::new(block0);
    let cond = dfg.make_block_param(block0, 0, Type::I32);
    block0_data.params.push(cond);

    let br = dfg.make_inst(Instruction::new(Opcode::CondBranch, vec![cond], Type::I32));
    block0_data.terminator = Some(br);
    block0_data.insts.push(br);
    layout.add_block(block0_data);

    let block1 = BlockId(1);
    let mut block1_data = Block::new(block1);
    let const10_1 = {
        let inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, 10);
        let id = dfg.make_inst(inst);
        block1_data.insts.push(id);
        dfg.make_inst_result(id, Type::I32)
    };
    let jump1 = dfg.make_inst(Instruction::new(Opcode::Branch, vec![const10_1], Type::I32));
    block1_data.terminator = Some(jump1);
    block1_data.insts.push(jump1);
    layout.add_block(block1_data);

    let block2 = BlockId(2);
    let mut block2_data = Block::new(block2);
    let const10_2 = {
        let inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, 10);
        let id = dfg.make_inst(inst);
        block2_data.insts.push(id);
        dfg.make_inst_result(id, Type::I32)
    };
    let jump2 = dfg.make_inst(Instruction::new(Opcode::Branch, vec![const10_2], Type::I32));
    block2_data.terminator = Some(jump2);
    block2_data.insts.push(jump2);
    layout.add_block(block2_data);

    let block3 = BlockId(3);
    let mut block3_data = Block::new(block3);
    let v3 = dfg.make_block_param(block3, 0, Type::I32); // PHI node!
    block3_data.params.push(v3);

    let five = {
        let inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, 5);
        let id = dfg.make_inst(inst);
        block3_data.insts.push(id);
        dfg.make_inst_result(id, Type::I32)
    };

    let add = {
        let inst = Instruction::new(Opcode::Add, vec![v3, five], Type::I32);
        let id = dfg.make_inst(inst);
        block3_data.insts.push(id);
        dfg.make_inst_result(id, Type::I32)
    };

    let ret = dfg.make_inst(Instruction::new(Opcode::Return, vec![add], Type::I32));
    block3_data.terminator = Some(ret);
    block3_data.insts.push(ret);
    layout.add_block(block3_data);

    // Run aegraph pass
    let domtree = DominatorTree::from_linear_blocks(&[block0, block1, block2, block3]);
    let mut pass = EgraphPass::new(dfg, layout, domtree);
    pass.run();
}

pub fn run_all_context_test_cases() {
    example_phi_constant_propagation();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn run_all() {
        run_all_context_test_cases();
    }
}
