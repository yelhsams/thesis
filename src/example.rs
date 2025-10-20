//! Complete example demonstrating the greenfield egraph pass
//!
//! This shows how to build a simple function, run the egraph pass,
//! and see the optimization in action.

use crate::egraph_pass::*;
use crate::elaborate::*;
use crate::support::*;
use crate::types::*;

/// Example 1: Simple algebraic optimization
///
/// Original code:
///   x = param
///   y = x + 0
///   z = y * 1
///   return z
///
/// After optimization:
///   x = param
///   return x
pub fn example_algebraic_simplification() {
    let mut dfg = DataFlowGraph::new();
    let mut layout = Layout::new();

    // Create single block
    let block_id = BlockId(0);
    let mut block = Block::new(block_id);

    // Block parameter: x
    let x = dfg.make_block_param(block_id, 0, Type::I32);
    block.params.push(x);

    println!("Building IR:");
    println!("  block0(x: i32):");

    // Instruction: y = x + 0
    let zero = {
        let inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, 0);
        let inst_id = dfg.make_inst(inst);
        println!("    v{} = iconst 0", inst_id.0);
        block.insts.push(inst_id);
        dfg.make_inst_result(inst_id, Type::I32)
    };

    let add_inst = Instruction::new(Opcode::Add, vec![x, zero], Type::I32);
    let add_id = dfg.make_inst(add_inst);
    let y = dfg.make_inst_result(add_id, Type::I32);
    println!(
        "    v{} = iadd x, v{}",
        add_id.0,
        dfg.insts.keys().next().unwrap().0
    );
    block.insts.push(add_id);

    // Instruction: z = y * 1
    let one = {
        let inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, 1);
        let inst_id = dfg.make_inst(inst);
        println!("    v{} = iconst 1", inst_id.0);
        block.insts.push(inst_id);
        dfg.make_inst_result(inst_id, Type::I32)
    };

    let mul_inst = Instruction::new(Opcode::Mul, vec![y, one], Type::I32);
    let mul_id = dfg.make_inst(mul_inst);
    let z = dfg.make_inst_result(mul_id, Type::I32);
    println!("    v{} = imul v{}, v{}", mul_id.0, y.0, one.0);
    block.insts.push(mul_id);

    // Return z
    let ret_inst = Instruction::new(Opcode::Return, vec![z], Type::I32);
    let ret_id = dfg.make_inst(ret_inst);
    println!("    return v{}", z.0);
    block.terminator = Some(ret_id);
    block.insts.push(ret_id);

    layout.add_block(block);

    // Run egraph pass
    let domtree = DominatorTree::from_linear_blocks(&[block_id]);
    let mut pass = EgraphPass::new(dfg, layout, domtree);
    pass.run();

    println!("\n✓ After optimization, y and z should both simplify to x");
    println!("  (x + 0) => x");
    println!("  (x * 1) => x");
}

/// Example 2: GVN (Global Value Numbering)
///
/// Original code:
///   x = param
///   y = param
///   a = x + y
///   b = x + y  // duplicate!
///   c = a + b
///
/// After optimization:
///   x = param
///   y = param
///   a = x + y
///   c = a + a  // b merged with a
pub fn example_gvn() {
    let mut dfg = DataFlowGraph::new();
    let mut layout = Layout::new();

    let block_id = BlockId(0);
    let mut block = Block::new(block_id);

    // Parameters
    let x = dfg.make_block_param(block_id, 0, Type::I32);
    let y = dfg.make_block_param(block_id, 1, Type::I32);
    block.params.extend(&[x, y]);

    println!("Building IR:");
    println!("  block0(x: i32, y: i32):");

    // a = x + y
    let add1_inst = Instruction::new(Opcode::Add, vec![x, y], Type::I32);
    let add1_id = dfg.make_inst(add1_inst);
    let a = dfg.make_inst_result(add1_id, Type::I32);
    println!("    v{} = iadd x, y", add1_id.0);
    block.insts.push(add1_id);

    // b = x + y (duplicate)
    let add2_inst = Instruction::new(Opcode::Add, vec![x, y], Type::I32);
    let add2_id = dfg.make_inst(add2_inst);
    let b = dfg.make_inst_result(add2_id, Type::I32);
    println!("    v{} = iadd x, y  // duplicate!", add2_id.0);
    block.insts.push(add2_id);

    // c = a + b
    let add3_inst = Instruction::new(Opcode::Add, vec![a, b], Type::I32);
    let add3_id = dfg.make_inst(add3_inst);
    let c = dfg.make_inst_result(add3_id, Type::I32);
    println!("    v{} = iadd v{}, v{}", add3_id.0, a.0, b.0);
    block.insts.push(add3_id);

    // Return c
    let ret_inst = Instruction::new(Opcode::Return, vec![c], Type::I32);
    let ret_id = dfg.make_inst(ret_inst);
    println!("    return v{}", c.0);
    block.terminator = Some(ret_id);
    block.insts.push(ret_id);

    layout.add_block(block);

    let domtree = DominatorTree::from_linear_blocks(&[block_id]);
    let mut pass = EgraphPass::new(dfg, layout, domtree);
    pass.run();

    println!("\n✓ The second 'iadd x, y' should be deduplicated");
    println!("  b merged with a via GVN");
}

/// Example 3: Union nodes and cost-based extraction
///
/// Shows how the egraph represents multiple equivalent forms
/// and chooses the best one.
pub fn example_union_and_extraction() {
    let mut dfg = DataFlowGraph::new();
    let mut layout = Layout::new();

    let block_id = BlockId(0);
    let mut block = Block::new(block_id);

    let x = dfg.make_block_param(block_id, 0, Type::I32);
    block.params.push(x);

    println!("Building IR with equivalent expressions:");
    println!("  block0(x: i32):");

    // Create: x + x (cost 1)
    let add_inst = Instruction::new(Opcode::Add, vec![x, x], Type::I32);
    let add_id = dfg.make_inst(add_inst);
    let add_val = dfg.make_inst_result(add_id, Type::I32);
    println!("    v{} = iadd x, x  // cost 1", add_id.0);
    block.insts.push(add_id);

    // Create: x * 2 (cost 2)
    let two = {
        let inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, 2);
        let inst_id = dfg.make_inst(inst);
        block.insts.push(inst_id);
        dfg.make_inst_result(inst_id, Type::I32)
    };

    let mul_inst = Instruction::new(Opcode::Mul, vec![x, two], Type::I32);
    let mul_id = dfg.make_inst(mul_inst);
    let mul_val = dfg.make_inst_result(mul_id, Type::I32);
    println!("    v{} = imul x, 2  // cost 2", mul_id.0);
    block.insts.push(mul_id);

    // Create union: these are semantically equivalent
    let union = dfg.make_union(add_val, mul_val);
    println!(
        "    v{} = union(v{}, v{})  // equivalent expressions",
        union.0, add_val.0, mul_val.0
    );

    // Use the union
    let ret_inst = Instruction::new(Opcode::Return, vec![union], Type::I32);
    let ret_id = dfg.make_inst(ret_inst);
    block.terminator = Some(ret_id);
    block.insts.push(ret_id);

    layout.add_block(block);

    println!("\nRunning elaboration to pick best form...");

    let domtree = DominatorTree::from_linear_blocks(&[block_id]);
    let mut stats = Stats::default();
    let mut elaborator = Elaborator::new(&mut dfg, &domtree, &mut stats);
    elaborator.elaborate(&layout);

    // Check which was chosen
    let best = elaborator.get_best_inst(union);
    println!("\n✓ Elaborator should choose 'iadd' (cost 1) over 'imul' (cost 2)");
    if let Some(inst_id) = best {
        let inst = &dfg.insts[&inst_id];
        println!("  Selected: {:?}", inst.opcode);
    }
}

/// Example 4: Scoped GVN (dominance-aware)
///
/// Shows how GVN respects dominance through scoped hash maps.
pub fn example_scoped_gvn() {
    let mut dfg = DataFlowGraph::new();
    let mut layout = Layout::new();

    // Block 0 (entry)
    let block0 = BlockId(0);
    let mut block0_data = Block::new(block0);
    let x = dfg.make_block_param(block0, 0, Type::I32);
    let y = dfg.make_block_param(block0, 1, Type::I32);
    block0_data.params.extend(&[x, y]);

    println!("Building CFG with multiple blocks:");
    println!("  block0(x: i32, y: i32):");

    // a = x + y in block0
    let add1_inst = Instruction::new(Opcode::Add, vec![x, y], Type::I32);
    let add1_id = dfg.make_inst(add1_inst);
    let _a = dfg.make_inst_result(add1_id, Type::I32);
    println!("    v{} = iadd x, y", add1_id.0);
    block0_data.insts.push(add1_id);

    // Branch to block1
    let br_inst = Instruction::new(Opcode::Branch, vec![], Type::I32);
    let br_id = dfg.make_inst(br_inst);
    println!("    jump block1");
    block0_data.terminator = Some(br_id);
    block0_data.insts.push(br_id);

    layout.add_block(block0_data);

    // Block 1 (child of block0)
    let block1 = BlockId(1);
    let mut block1_data = Block::new(block1);

    println!("  block1:");

    // b = x + y in block1 (should merge with a via scoped GVN)
    let add2_inst = Instruction::new(Opcode::Add, vec![x, y], Type::I32);
    let add2_id = dfg.make_inst(add2_inst);
    let b = dfg.make_inst_result(add2_id, Type::I32);
    println!(
        "    v{} = iadd x, y  // should merge with block0's iadd",
        add2_id.0
    );
    block1_data.insts.push(add2_id);

    let ret_inst = Instruction::new(Opcode::Return, vec![b], Type::I32);
    let ret_id = dfg.make_inst(ret_inst);
    println!("    return v{}", b.0);
    block1_data.terminator = Some(ret_id);
    block1_data.insts.push(ret_id);

    layout.add_block(block1_data);

    let domtree = DominatorTree::from_linear_blocks(&[block0, block1]);
    let mut pass = EgraphPass::new(dfg, layout, domtree);
    pass.run();

    println!("\n✓ GVN should merge both 'iadd x, y' across blocks");
    println!("  Scoped hashmap ensures dominance is respected");
}

/// Run all examples
pub fn run_all_examples() {
    println!("\n");

    example_algebraic_simplification();
    example_gvn();
    example_union_and_extraction();
    example_scoped_gvn();

    println!("\n");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_all_examples() {
        example_algebraic_simplification();
        example_gvn();
        example_union_and_extraction();
        example_scoped_gvn();
    }
}
