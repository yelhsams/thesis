//! Comprehensive examples of pattern-based rewrite rules
//!
//! This demonstrates the full power of the declarative pattern IR
//! for specifying optimization rules.

use crate::pattern::*;
use crate::rewrite_integration::*;
use crate::types::*;

/// Example 1: Basic algebraic simplification with patterns
pub fn example_basic_pattern_rewrites() {
    println!("\n=== Example 1: Basic Pattern Rewrites ===\n");

    let mut dfg = DataFlowGraph::new();
    let block = BlockId(0);

    // Create: x = param
    let x = dfg.make_block_param(block, 0, Type::I32);
    println!("Created: x (block parameter)");

    // Create: zero = const 0
    let const_inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, 0);
    let const_id = dfg.make_inst(const_inst);
    let zero = dfg.make_inst_result(const_id, Type::I32);
    println!("Created: zero = const 0");

    // Create: y = x + 0
    let add_inst = Instruction::new(Opcode::Add, vec![x, zero], Type::I32);
    let add_id = dfg.make_inst(add_inst);
    let y = dfg.make_inst_result(add_id, Type::I32);
    println!("Created: y = x + 0");

    // Apply pattern-based rewrites
    let mut engine = RewriteEngine::with_standard_library();
    println!("\nApplying pattern-based rewrites...");
    let results = engine.apply_rewrites(&mut dfg, y);

    println!("\nRewrite produced {} equivalent values", results.len());
    println!("✓ Expected: x + 0 => x");
}

/// Example 2: Strength reduction with patterns
pub fn example_strength_reduction() {
    println!("\n=== Example 2: Strength Reduction ===\n");

    let mut dfg = DataFlowGraph::new();
    let block = BlockId(0);
    let x = dfg.make_block_param(block, 0, Type::I32);

    // Create: two = const 2
    let two_inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, 2);
    let two_id = dfg.make_inst(two_inst);
    let two = dfg.make_inst_result(two_id, Type::I32);

    // Create: y = x * 2
    let mul_inst = Instruction::new(Opcode::Mul, vec![x, two], Type::I32);
    let mul_id = dfg.make_inst(mul_inst);
    let y = dfg.make_inst_result(mul_id, Type::I32);

    println!("Created: y = x * 2");

    // Apply rewrites
    let mut engine = RewriteEngine::with_standard_library();
    println!("\nApplying strength reduction...");
    let results = engine.apply_rewrites(&mut dfg, y);

    println!("\n✓ Expected: x * 2 => x + x (cheaper operation)");
    println!("  Rewrites produced: {} alternatives", results.len());
}

/// Example 3: Custom domain-specific patterns
pub fn example_custom_patterns() {
    println!("\n=== Example 3: Custom Domain-Specific Patterns ===\n");

    // Build a custom rule for bitwise operations
    let custom_rule = Rewrite::new("and-with-minus-one")
        .match_pattern(Pattern::op(
            Opcode::And,
            vec![Pattern::var("x"), Pattern::constant(-1)],
        ))
        .produce(Pattern::var("x"))
        .build();

    println!("Created custom rule: 'and-with-minus-one'");
    println!("  Pattern: (x & -1) => x");
    println!("  Rationale: ANDing with all 1s is identity");

    // Create another rule with conditions
    let power_of_two_rule = Rewrite::new("mul-power-of-two")
        .match_pattern(Pattern::op(
            Opcode::Mul,
            vec![Pattern::var("x"), Pattern::any_constant("c")],
        ))
        .produce(Pattern::op(
            Opcode::Mul,
            vec![Pattern::var("x"), Pattern::any_constant("c")], // Would be shift in real impl
        ))
        .when(Condition::Custom {
            name: "is-power-of-two".to_string(),
            check: |bindings| {
                if let Some(c) = bindings.get_constant(&VarId::new("c")) {
                    c > 0 && (c & (c - 1)) == 0 // Check if power of 2
                } else {
                    false
                }
            },
        })
        .build();

    println!("\nCreated conditional rule: 'mul-power-of-two'");
    println!("  Pattern: (x * c) => (x << log2(c))  [when c is power of 2]");
    println!("  This demonstrates conditions in patterns");
}

/// Example 4: Associativity and commutativity
pub fn example_algebraic_laws() {
    println!("\n=== Example 4: Algebraic Laws (Associativity & Commutativity) ===\n");

    // Commutativity: x + y <=> y + x
    let commute_add = Rewrite::new("commute-add")
        .match_pattern(Pattern::op(
            Opcode::Add,
            vec![Pattern::var("x"), Pattern::var("y")],
        ))
        .produce(Pattern::op(
            Opcode::Add,
            vec![Pattern::var("y"), Pattern::var("x")],
        ))
        .bidirectional()
        .build();

    println!("Commutativity rule:");
    println!("  (x + y) <=> (y + x)");
    println!("  Bidirectional: {}", commute_add.bidirectional);

    // Associativity: (x + y) + z => x + (y + z)
    let assoc_add = Rewrite::new("assoc-add")
        .match_pattern(Pattern::op(
            Opcode::Add,
            vec![
                Pattern::op(Opcode::Add, vec![Pattern::var("x"), Pattern::var("y")]),
                Pattern::var("z"),
            ],
        ))
        .produce(Pattern::op(
            Opcode::Add,
            vec![
                Pattern::var("x"),
                Pattern::op(Opcode::Add, vec![Pattern::var("y"), Pattern::var("z")]),
            ],
        ))
        .bidirectional()
        .build();

    println!("\nAssociativity rule:");
    println!("  ((x + y) + z) <=> (x + (y + z))");
    println!("  Bidirectional: {}", assoc_add.bidirectional);
}

/// Example 5: Constant folding with pattern matching
pub fn example_constant_folding() {
    println!("\n=== Example 5: Constant Folding ===\n");

    let mut dfg = DataFlowGraph::new();

    // Create: a = const 5
    let a_inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, 5);
    let a_id = dfg.make_inst(a_inst);
    let a = dfg.make_inst_result(a_id, Type::I32);

    // Create: b = const 3
    let b_inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, 3);
    let b_id = dfg.make_inst(b_inst);
    let b = dfg.make_inst_result(b_id, Type::I32);

    // Create: c = a + b (should fold to 8)
    let add_inst = Instruction::new(Opcode::Add, vec![a, b], Type::I32);
    let add_id = dfg.make_inst(add_inst);
    let c = dfg.make_inst_result(add_id, Type::I32);

    println!("Created: a = const 5");
    println!("Created: b = const 3");
    println!("Created: c = a + b");

    // Define constant folding rule
    let fold_rule = Rewrite::new("fold-add-constants")
        .match_pattern(Pattern::op(
            Opcode::Add,
            vec![Pattern::any_constant("a"), Pattern::any_constant("b")],
        ))
        .produce(Pattern::any_constant("result"))
        .when(Condition::And(vec![
            Condition::IsConstant(VarId::new("a")),
            Condition::IsConstant(VarId::new("b")),
        ]))
        .build();

    println!("\nConstant folding rule:");
    println!("  Pattern: (const a) + (const b) => const (a + b)");
    println!("  When: both operands are constants");
}

/// Example 6: Nested pattern matching
pub fn example_nested_patterns() {
    println!("\n=== Example 6: Nested Pattern Matching ===\n");

    // Example: Simplify (x + 0) * 1 => x
    let nested_rule = Rewrite::new("nested-simplify")
        .match_pattern(Pattern::op(
            Opcode::Mul,
            vec![
                Pattern::op(Opcode::Add, vec![Pattern::var("x"), Pattern::constant(0)]),
                Pattern::constant(1),
            ],
        ))
        .produce(Pattern::var("x"))
        .build();

    println!("Nested simplification rule:");
    println!("  Pattern: ((x + 0) * 1) => x");
    println!("  Matches nested expression structure");

    // Example: Distributive law with nested patterns
    let distributive = Rewrite::new("distributive")
        .match_pattern(Pattern::op(
            Opcode::Mul,
            vec![
                Pattern::var("a"),
                Pattern::op(Opcode::Add, vec![Pattern::var("b"), Pattern::var("c")]),
            ],
        ))
        .produce(Pattern::op(
            Opcode::Add,
            vec![
                Pattern::op(Opcode::Mul, vec![Pattern::var("a"), Pattern::var("b")]),
                Pattern::op(Opcode::Mul, vec![Pattern::var("a"), Pattern::var("c")]),
            ],
        ))
        .build();

    println!("\nDistributive law:");
    println!("  Pattern: a * (b + c) => (a * b) + (a * c)");
    println!("  Demonstrates complex nested rewriting");
}

/// Example 7: Using pattern alternatives (OR patterns)
pub fn example_pattern_alternatives() {
    println!("\n=== Example 7: Pattern Alternatives (OR) ===\n");

    // Match either x + 0 OR 0 + x
    let add_zero_rule = Rewrite::new("add-zero-either-side")
        .match_pattern(Pattern::Or(vec![
            Pattern::op(Opcode::Add, vec![Pattern::var("x"), Pattern::constant(0)]),
            Pattern::op(Opcode::Add, vec![Pattern::constant(0), Pattern::var("x")]),
        ]))
        .produce(Pattern::var("x"))
        .build();

    println!("Rule with alternatives:");
    println!("  Pattern: (x + 0) OR (0 + x) => x");
    println!("  Matches either ordering");

    // Another example: x * 1 or 1 * x or x * -1 or -1 * x
    println!("\nMore complex alternatives:");
    println!("  Pattern: (x * 1) OR (1 * x) => x");
    println!("  Pattern: (x * 0) OR (0 * x) => 0");
}

/// Example 8: Conditional patterns with Where clauses
pub fn example_conditional_patterns() {
    println!("\n=== Example 8: Conditional Patterns ===\n");

    // Match x * c where c > 1 (optimization only makes sense for larger constants)
    let strength_reduce = Rewrite::new("strength-reduce-large-mul")
        .match_pattern(Pattern::Where {
            pattern: Box::new(Pattern::op(
                Opcode::Mul,
                vec![Pattern::var("x"), Pattern::any_constant("c")],
            )),
            condition: Condition::GreaterThan(VarId::new("c"), 1),
        })
        .produce(Pattern::op(
            Opcode::Mul,
            vec![Pattern::var("x"), Pattern::any_constant("c")],
        ))
        .build();

    println!("Conditional pattern:");
    println!("  Pattern: (x * c) WHERE c > 1");
    println!("  Only applies optimization for larger multipliers");

    // Example with inequality checks
    println!("\nAvailable conditions:");
    println!("  - IsConstant(var)");
    println!("  - EqualsConstant(var, value)");
    println!("  - NotEqualsConstant(var, value)");
    println!("  - GreaterThan(var, value)");
    println!("  - LessThan(var, value)");
    println!("  - VarsEqual(var1, var2)");
    println!("  - VarsNotEqual(var1, var2)");
    println!("  - And([conditions...])");
    println!("  - Or([conditions...])");
    println!("  - Custom(function)");
}

/// Example 9: Building a complete optimization pipeline
pub fn example_optimization_pipeline() {
    println!("\n=== Example 9: Complete Optimization Pipeline ===\n");

    let mut dfg = DataFlowGraph::new();
    let block = BlockId(0);
    let x = dfg.make_block_param(block, 0, Type::I32);

    // Build complex expression: ((x + 0) * 2) + 0
    let zero = {
        let inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, 0);
        let id = dfg.make_inst(inst);
        dfg.make_inst_result(id, Type::I32)
    };

    let add1 = {
        let inst = Instruction::new(Opcode::Add, vec![x, zero], Type::I32);
        let id = dfg.make_inst(inst);
        dfg.make_inst_result(id, Type::I32)
    };

    let two = {
        let inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, 2);
        let id = dfg.make_inst(inst);
        dfg.make_inst_result(id, Type::I32)
    };

    let mul = {
        let inst = Instruction::new(Opcode::Mul, vec![add1, two], Type::I32);
        let id = dfg.make_inst(inst);
        dfg.make_inst_result(id, Type::I32)
    };

    let final_expr = {
        let inst = Instruction::new(Opcode::Add, vec![mul, zero], Type::I32);
        let id = dfg.make_inst(inst);
        dfg.make_inst_result(id, Type::I32)
    };

    println!("Created expression: ((x + 0) * 2) + 0");
    println!("\nApplying optimization pipeline...");

    // Apply multiple rounds of rewrites
    let mut engine = RewriteEngine::with_standard_library();

    for round in 0..3 {
        println!("\nRound {}:", round + 1);
        let results = engine.apply_rewrites(&mut dfg, final_expr);
        println!("  Produced {} new equivalences", results.len());
    }

    println!("\n✓ Expected final result: x + x");
    println!("  (x + 0) => x");
    println!("  (x * 2) => (x + x)");
    println!("  ((x + x) + 0) => (x + x)");
}

/// Run all pattern examples
pub fn run_all_pattern_examples() {
    println!("\n╔════════════════════════════════════════════════════════════╗");
    println!("║       Pattern-Based Rewrite Rules: Complete Examples       ║");
    println!("╚════════════════════════════════════════════════════════════╝");

    example_basic_pattern_rewrites();
    example_strength_reduction();
    example_custom_patterns();
    example_algebraic_laws();
    example_constant_folding();
    example_nested_patterns();
    example_pattern_alternatives();
    example_conditional_patterns();
    example_optimization_pipeline();

    println!("\n╔════════════════════════════════════════════════════════════╗");
    println!("║                      All Examples Complete                  ║");
    println!("╚════════════════════════════════════════════════════════════╝\n");
}

/// Example 10: Comparing hardcoded vs pattern-based rules
pub fn example_comparison() {
    println!("\n=== Hardcoded vs Pattern-Based Rules ===\n");

    println!("HARDCODED APPROACH:");
    println!("```rust");
    println!("fn apply_add_zero(value: ValueId, dfg: &DataFlowGraph) -> Option<ValueId> {{");
    println!("    if let ValueDef::Inst(inst_id) = dfg.value_def(value) {{");
    println!("        let inst = &dfg.insts[&inst_id];");
    println!("        if inst.opcode == Opcode::Add {{");
    println!("            if is_zero(&inst.args[1], dfg) {{");
    println!("                return Some(inst.args[0]);");
    println!("            }}");
    println!("        }}");
    println!("    }}");
    println!("    None");
    println!("}}");
    println!("```");

    println!("\nPATTERN-BASED APPROACH:");
    println!("```rust");
    println!("let rule = Rewrite::new(\"add-zero\")");
    println!("    .match_pattern(Pattern::op(");
    println!("        Opcode::Add,");
    println!("        vec![Pattern::var(\"x\"), Pattern::constant(0)]");
    println!("    ))");
    println!("    .produce(Pattern::var(\"x\"))");
    println!("    .build();");
    println!("```");

    println!("\nBENEFITS OF PATTERN-BASED:");
    println!("  ✓ Declarative and readable");
    println!("  ✓ Composable (can build complex patterns)");
    println!("  ✓ Easier to verify correctness");
    println!("  ✓ Can be loaded from config files");
    println!("  ✓ Supports bidirectional rules");
    println!("  ✓ Built-in condition checking");
    println!("  ✓ Reduces code duplication");
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::range::RangeAssumptions;

    #[test]
    fn test_all_pattern_examples() {
        example_basic_pattern_rewrites();
        example_strength_reduction();
        example_custom_patterns();
        example_algebraic_laws();
        example_constant_folding();
        example_nested_patterns();
        example_pattern_alternatives();
        example_conditional_patterns();
        example_optimization_pipeline();
        example_comparison();
    }

    /// Helper: create a const value in a DFG
    fn make_const(dfg: &mut DataFlowGraph, val: i64) -> ValueId {
        let inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, val);
        let id = dfg.make_inst(inst);
        dfg.make_inst_result(id, Type::I32)
    }

    /// Helper: create an op with given args
    fn make_op(dfg: &mut DataFlowGraph, opcode: Opcode, args: Vec<ValueId>) -> ValueId {
        let inst = Instruction::new(opcode, args, Type::I32);
        let id = dfg.make_inst(inst);
        dfg.make_inst_result(id, Type::I32)
    }

    /// Helper: check if a rewrite engine can rewrite `value` and one of the results
    /// is a constant with `expected_val`.
    fn rewrites_to_const(dfg: &mut DataFlowGraph, value: ValueId, expected_val: i64) -> bool {
        let mut engine = RewriteEngine::with_standard_library();
        let results = engine.apply_rewrites(dfg, value);
        // Check if any result is a constant instruction with the expected value
        for &v in &results {
            if let ValueDef::Inst(inst_id) = dfg.value_def(v) {
                let inst = &dfg.insts[&inst_id];
                if inst.opcode == Opcode::Const && inst.immediate == Some(expected_val) {
                    return true;
                }
            }
        }
        false
    }

    /// Helper: check if any rewrite produces a node with the given opcode
    fn rewrites_to_opcode(dfg: &mut DataFlowGraph, value: ValueId, opcode: Opcode) -> bool {
        let mut engine = RewriteEngine::with_standard_library();
        let results = engine.apply_rewrites(dfg, value);
        for &v in &results {
            if let ValueDef::Inst(inst_id) = dfg.value_def(v) {
                let inst = &dfg.insts[&inst_id];
                if inst.opcode == opcode {
                    return true;
                }
            }
        }
        false
    }

    /// Helper: check if any rewrite produces the exact value (e.g., a parameter)
    fn rewrites_to_value(dfg: &mut DataFlowGraph, value: ValueId, target: ValueId) -> bool {
        let mut engine = RewriteEngine::with_standard_library();
        let results = engine.apply_rewrites(dfg, value);
        results.contains(&target)
    }

    // ===== Test 1: Arithmetic ISLE Rules =====
    #[test]
    fn test_arithmetic_isle_rules() {
        println!("\n=== Test: Arithmetic ISLE Rules ===");

        // 1. ineg(x) => sub(0, x)
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let neg_x = make_op(&mut dfg, Opcode::Ineg, vec![x]);
            assert!(rewrites_to_opcode(&mut dfg, neg_x, Opcode::Sub),
                    "ineg(x) should rewrite to sub(0, x)");
            println!("  ineg(x) => sub(0, x)");
        }

        // 2. sub(0, x) => ineg(x)
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let zero = make_const(&mut dfg, 0);
            let sub0x = make_op(&mut dfg, Opcode::Sub, vec![zero, x]);
            assert!(rewrites_to_opcode(&mut dfg, sub0x, Opcode::Ineg),
                    "sub(0, x) should rewrite to ineg(x)");
            println!("  sub(0, x) => ineg(x)");
        }

        // 3. add(x, ineg(y)) => sub(x, y)
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let y = dfg.make_block_param(block, 1, Type::I32);
            let neg_y = make_op(&mut dfg, Opcode::Ineg, vec![y]);
            let add_neg = make_op(&mut dfg, Opcode::Add, vec![x, neg_y]);
            assert!(rewrites_to_opcode(&mut dfg, add_neg, Opcode::Sub),
                    "add(x, ineg(y)) should rewrite to sub(x, y)");
            println!("  add(x, ineg(y)) => sub(x, y)");
        }

        // 4. sub(add(x, y), y) => x
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let y = dfg.make_block_param(block, 1, Type::I32);
            let add_xy = make_op(&mut dfg, Opcode::Add, vec![x, y]);
            let sub_y = make_op(&mut dfg, Opcode::Sub, vec![add_xy, y]);
            assert!(rewrites_to_value(&mut dfg, sub_y, x),
                    "sub(add(x, y), y) should rewrite to x");
            println!("  sub(add(x, y), y) => x");
        }

        // 5. mul(x, 4) => shl(x, 2)
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let four = make_const(&mut dfg, 4);
            let mul4 = make_op(&mut dfg, Opcode::Mul, vec![x, four]);
            assert!(rewrites_to_opcode(&mut dfg, mul4, Opcode::Shl),
                    "mul(x, 4) should rewrite to shl(x, 2)");
            println!("  mul(x, 4) => shl(x, 2)");
        }

        // 6. sdiv(x, 1) => x
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let one = make_const(&mut dfg, 1);
            let sdiv1 = make_op(&mut dfg, Opcode::Sdiv, vec![x, one]);
            assert!(rewrites_to_value(&mut dfg, sdiv1, x),
                    "sdiv(x, 1) should rewrite to x");
            println!("  sdiv(x, 1) => x");
        }

        // 7. urem(x, 1) => 0
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let one = make_const(&mut dfg, 1);
            let urem1 = make_op(&mut dfg, Opcode::Urem, vec![x, one]);
            assert!(rewrites_to_const(&mut dfg, urem1, 0),
                    "urem(x, 1) should rewrite to 0");
            println!("  urem(x, 1) => 0");
        }

        // 8. select(1, x, y) => x
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let y = dfg.make_block_param(block, 1, Type::I32);
            let one = make_const(&mut dfg, 1);
            let sel = make_op(&mut dfg, Opcode::Select, vec![one, x, y]);
            assert!(rewrites_to_value(&mut dfg, sel, x),
                    "select(1, x, y) should rewrite to x");
            println!("  select(1, x, y) => x");
        }

        // 9. select(0, x, y) => y
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let y = dfg.make_block_param(block, 1, Type::I32);
            let zero = make_const(&mut dfg, 0);
            let sel = make_op(&mut dfg, Opcode::Select, vec![zero, x, y]);
            assert!(rewrites_to_value(&mut dfg, sel, y),
                    "select(0, x, y) should rewrite to y");
            println!("  select(0, x, y) => y");
        }

        // 10. bnot(x) + 1 => ineg(x)
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let bnot_x = make_op(&mut dfg, Opcode::Bnot, vec![x]);
            let one = make_const(&mut dfg, 1);
            let add1 = make_op(&mut dfg, Opcode::Add, vec![bnot_x, one]);
            assert!(rewrites_to_opcode(&mut dfg, add1, Opcode::Ineg),
                    "add(bnot(x), 1) should rewrite to ineg(x)");
            println!("  add(bnot(x), 1) => ineg(x)");
        }

        println!("  All arithmetic ISLE rule tests passed!");
    }

    // ===== Test 2: Constant Folding ISLE Rules =====
    #[test]
    fn test_const_fold_isle() {
        println!("\n=== Test: Constant Folding ISLE Rules ===");

        // 1. add(5, 3) => 8
        {
            let mut dfg = DataFlowGraph::new();
            let a = make_const(&mut dfg, 5);
            let b = make_const(&mut dfg, 3);
            let add = make_op(&mut dfg, Opcode::Add, vec![a, b]);
            assert!(rewrites_to_const(&mut dfg, add, 8),
                    "add(5, 3) should fold to 8");
            println!("  add(5, 3) => 8");
        }

        // 2. sub(10, 4) => 6
        {
            let mut dfg = DataFlowGraph::new();
            let a = make_const(&mut dfg, 10);
            let b = make_const(&mut dfg, 4);
            let sub = make_op(&mut dfg, Opcode::Sub, vec![a, b]);
            assert!(rewrites_to_const(&mut dfg, sub, 6),
                    "sub(10, 4) should fold to 6");
            println!("  sub(10, 4) => 6");
        }

        // 3. mul(6, 7) => 42
        {
            let mut dfg = DataFlowGraph::new();
            let a = make_const(&mut dfg, 6);
            let b = make_const(&mut dfg, 7);
            let mul = make_op(&mut dfg, Opcode::Mul, vec![a, b]);
            assert!(rewrites_to_const(&mut dfg, mul, 42),
                    "mul(6, 7) should fold to 42");
            println!("  mul(6, 7) => 42");
        }

        // 4. and(0xFF, 0x0F) => 0x0F
        {
            let mut dfg = DataFlowGraph::new();
            let a = make_const(&mut dfg, 0xFF);
            let b = make_const(&mut dfg, 0x0F);
            let and = make_op(&mut dfg, Opcode::And, vec![a, b]);
            assert!(rewrites_to_const(&mut dfg, and, 0x0F),
                    "and(0xFF, 0x0F) should fold to 0x0F");
            println!("  and(0xFF, 0x0F) => 0x0F");
        }

        // 5. or(0xF0, 0x0F) => 0xFF
        {
            let mut dfg = DataFlowGraph::new();
            let a = make_const(&mut dfg, 0xF0);
            let b = make_const(&mut dfg, 0x0F);
            let or = make_op(&mut dfg, Opcode::Or, vec![a, b]);
            assert!(rewrites_to_const(&mut dfg, or, 0xFF),
                    "or(0xF0, 0x0F) should fold to 0xFF");
            println!("  or(0xF0, 0x0F) => 0xFF");
        }

        // 6. xor(0xFF, 0x0F) => 0xF0
        {
            let mut dfg = DataFlowGraph::new();
            let a = make_const(&mut dfg, 0xFF);
            let b = make_const(&mut dfg, 0x0F);
            let xor = make_op(&mut dfg, Opcode::Xor, vec![a, b]);
            assert!(rewrites_to_const(&mut dfg, xor, 0xF0),
                    "xor(0xFF, 0x0F) should fold to 0xF0");
            println!("  xor(0xFF, 0x0F) => 0xF0");
        }

        // 7. shl(1, 3) => 8
        {
            let mut dfg = DataFlowGraph::new();
            let a = make_const(&mut dfg, 1);
            let b = make_const(&mut dfg, 3);
            let shl = make_op(&mut dfg, Opcode::Shl, vec![a, b]);
            assert!(rewrites_to_const(&mut dfg, shl, 8),
                    "shl(1, 3) should fold to 8");
            println!("  shl(1, 3) => 8");
        }

        // 8. ineg(5) => -5
        {
            let mut dfg = DataFlowGraph::new();
            let a = make_const(&mut dfg, 5);
            let neg = make_op(&mut dfg, Opcode::Ineg, vec![a]);
            assert!(rewrites_to_const(&mut dfg, neg, -5),
                    "ineg(5) should fold to -5");
            println!("  ineg(5) => -5");
        }

        // 9. bnot(0) => -1
        {
            let mut dfg = DataFlowGraph::new();
            let a = make_const(&mut dfg, 0);
            let bnot = make_op(&mut dfg, Opcode::Bnot, vec![a]);
            assert!(rewrites_to_const(&mut dfg, bnot, -1),
                    "bnot(0) should fold to -1");
            println!("  bnot(0) => -1");
        }

        // 10. iabs(-7) => 7
        {
            let mut dfg = DataFlowGraph::new();
            let a = make_const(&mut dfg, -7);
            let abs = make_op(&mut dfg, Opcode::Iabs, vec![a]);
            assert!(rewrites_to_const(&mut dfg, abs, 7),
                    "iabs(-7) should fold to 7");
            println!("  iabs(-7) => 7");
        }

        // 11. eq(5, 5) => 1
        {
            let mut dfg = DataFlowGraph::new();
            let a = make_const(&mut dfg, 5);
            let b = make_const(&mut dfg, 5);
            let eq = make_op(&mut dfg, Opcode::Eq, vec![a, b]);
            assert!(rewrites_to_const(&mut dfg, eq, 1),
                    "eq(5, 5) should fold to 1");
            println!("  eq(5, 5) => 1");
        }

        // 12. slt(3, 5) => 1
        {
            let mut dfg = DataFlowGraph::new();
            let a = make_const(&mut dfg, 3);
            let b = make_const(&mut dfg, 5);
            let slt = make_op(&mut dfg, Opcode::Slt, vec![a, b]);
            assert!(rewrites_to_const(&mut dfg, slt, 1),
                    "slt(3, 5) should fold to 1");
            println!("  slt(3, 5) => 1");
        }

        // 13. clz(0) => 64
        {
            let mut dfg = DataFlowGraph::new();
            let a = make_const(&mut dfg, 0);
            let clz = make_op(&mut dfg, Opcode::Clz, vec![a]);
            assert!(rewrites_to_const(&mut dfg, clz, 64),
                    "clz(0) should fold to 64");
            println!("  clz(0) => 64");
        }

        // 14. ctz(8) => 3
        {
            let mut dfg = DataFlowGraph::new();
            let a = make_const(&mut dfg, 8);
            let ctz = make_op(&mut dfg, Opcode::Ctz, vec![a]);
            assert!(rewrites_to_const(&mut dfg, ctz, 3),
                    "ctz(8) should fold to 3");
            println!("  ctz(8) => 3");
        }

        println!("  All constant folding ISLE rule tests passed!");
    }

    // ===== Test 3: Icmp Simplification ISLE Rules =====
    #[test]
    fn test_icmp_isle_rules() {
        println!("\n=== Test: Icmp Simplification ISLE Rules ===");

        // 1. ult(x, 0) => 0
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let zero = make_const(&mut dfg, 0);
            let ult0 = make_op(&mut dfg, Opcode::Ult, vec![x, zero]);
            assert!(rewrites_to_const(&mut dfg, ult0, 0),
                    "ult(x, 0) should rewrite to 0");
            println!("  ult(x, 0) => 0");
        }

        // 2. uge(x, 0) => 1
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let zero = make_const(&mut dfg, 0);
            let uge0 = make_op(&mut dfg, Opcode::Uge, vec![x, zero]);
            assert!(rewrites_to_const(&mut dfg, uge0, 1),
                    "uge(x, 0) should rewrite to 1");
            println!("  uge(x, 0) => 1");
        }

        // 3. eq(eq(x, y), 0) => ne(x, y)
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let y = dfg.make_block_param(block, 1, Type::I32);
            let eq_xy = make_op(&mut dfg, Opcode::Eq, vec![x, y]);
            let zero = make_const(&mut dfg, 0);
            let eq0 = make_op(&mut dfg, Opcode::Eq, vec![eq_xy, zero]);
            assert!(rewrites_to_opcode(&mut dfg, eq0, Opcode::Ne),
                    "eq(eq(x, y), 0) should rewrite to ne(x, y)");
            println!("  eq(eq(x, y), 0) => ne(x, y)");
        }

        // 4. ne(eq(x, y), 0) => eq(x, y)
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let y = dfg.make_block_param(block, 1, Type::I32);
            let eq_xy = make_op(&mut dfg, Opcode::Eq, vec![x, y]);
            let zero = make_const(&mut dfg, 0);
            let ne0 = make_op(&mut dfg, Opcode::Ne, vec![eq_xy, zero]);
            // ne(eq(x,y), 0) => eq(x,y) — the result should be eq_xy itself
            assert!(rewrites_to_opcode(&mut dfg, ne0, Opcode::Eq),
                    "ne(eq(x, y), 0) should rewrite to eq(x, y)");
            println!("  ne(eq(x, y), 0) => eq(x, y)");
        }

        // 5. eq(bnot(x), bnot(y)) => eq(x, y)
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let y = dfg.make_block_param(block, 1, Type::I32);
            let bx = make_op(&mut dfg, Opcode::Bnot, vec![x]);
            let by = make_op(&mut dfg, Opcode::Bnot, vec![y]);
            let eq_bb = make_op(&mut dfg, Opcode::Eq, vec![bx, by]);
            assert!(rewrites_to_opcode(&mut dfg, eq_bb, Opcode::Eq),
                    "eq(bnot(x), bnot(y)) should simplify");
            println!("  eq(bnot(x), bnot(y)) => eq(x, y)");
        }

        // 6. eq(add(x, c), add(y, c)) => eq(x, y)
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let y = dfg.make_block_param(block, 1, Type::I32);
            let c = dfg.make_block_param(block, 2, Type::I32);
            let add_xc = make_op(&mut dfg, Opcode::Add, vec![x, c]);
            let add_yc = make_op(&mut dfg, Opcode::Add, vec![y, c]);
            let eq_add = make_op(&mut dfg, Opcode::Eq, vec![add_xc, add_yc]);
            assert!(rewrites_to_opcode(&mut dfg, eq_add, Opcode::Eq),
                    "eq(add(x, c), add(y, c)) should simplify");
            println!("  eq(add(x, c), add(y, c)) => eq(x, y)");
        }

        // 7. ule(0, x) => 1
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let zero = make_const(&mut dfg, 0);
            let ule = make_op(&mut dfg, Opcode::Ule, vec![zero, x]);
            assert!(rewrites_to_const(&mut dfg, ule, 1),
                    "ule(0, x) should rewrite to 1");
            println!("  ule(0, x) => 1");
        }

        // 8. ugt(0, x) => 0
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let zero = make_const(&mut dfg, 0);
            let ugt = make_op(&mut dfg, Opcode::Ugt, vec![zero, x]);
            assert!(rewrites_to_const(&mut dfg, ugt, 0),
                    "ugt(0, x) should rewrite to 0");
            println!("  ugt(0, x) => 0");
        }

        println!("  All icmp ISLE rule tests passed!");
    }

    // ===== Test 4: Extend/Reduce ISLE Rules =====
    #[test]
    fn test_extend_isle_rules() {
        println!("\n=== Test: Extend/Reduce ISLE Rules ===");

        // 1. uextend(uextend(x)) => uextend(x)
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let ext1 = make_op(&mut dfg, Opcode::Uextend, vec![x]);
            let ext2 = make_op(&mut dfg, Opcode::Uextend, vec![ext1]);
            assert!(rewrites_to_opcode(&mut dfg, ext2, Opcode::Uextend),
                    "uextend(uextend(x)) should collapse");
            println!("  uextend(uextend(x)) => uextend(x)");
        }

        // 2. sextend(sextend(x)) => sextend(x)
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let ext1 = make_op(&mut dfg, Opcode::Sextend, vec![x]);
            let ext2 = make_op(&mut dfg, Opcode::Sextend, vec![ext1]);
            assert!(rewrites_to_opcode(&mut dfg, ext2, Opcode::Sextend),
                    "sextend(sextend(x)) should collapse");
            println!("  sextend(sextend(x)) => sextend(x)");
        }

        // 3. ireduce(ireduce(x)) => ireduce(x)
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let red1 = make_op(&mut dfg, Opcode::Ireduce, vec![x]);
            let red2 = make_op(&mut dfg, Opcode::Ireduce, vec![red1]);
            assert!(rewrites_to_opcode(&mut dfg, red2, Opcode::Ireduce),
                    "ireduce(ireduce(x)) should collapse");
            println!("  ireduce(ireduce(x)) => ireduce(x)");
        }

        // 4. ireduce(uextend(x)) => x
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let ext = make_op(&mut dfg, Opcode::Uextend, vec![x]);
            let red = make_op(&mut dfg, Opcode::Ireduce, vec![ext]);
            assert!(rewrites_to_value(&mut dfg, red, x),
                    "ireduce(uextend(x)) should rewrite to x");
            println!("  ireduce(uextend(x)) => x");
        }

        // 5. ireduce(sextend(x)) => x
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let ext = make_op(&mut dfg, Opcode::Sextend, vec![x]);
            let red = make_op(&mut dfg, Opcode::Ireduce, vec![ext]);
            assert!(rewrites_to_value(&mut dfg, red, x),
                    "ireduce(sextend(x)) should rewrite to x");
            println!("  ireduce(sextend(x)) => x");
        }

        // 6. and(uextend(x), uextend(y)) => uextend(and(x, y))
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let y = dfg.make_block_param(block, 1, Type::I32);
            let ext_x = make_op(&mut dfg, Opcode::Uextend, vec![x]);
            let ext_y = make_op(&mut dfg, Opcode::Uextend, vec![y]);
            let and = make_op(&mut dfg, Opcode::And, vec![ext_x, ext_y]);
            assert!(rewrites_to_opcode(&mut dfg, and, Opcode::Uextend),
                    "and(uextend(x), uextend(y)) should push under extend");
            println!("  and(uextend(x), uextend(y)) => uextend(and(x, y))");
        }

        // 7. eq(uextend(x), uextend(y)) => eq(x, y)
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let y = dfg.make_block_param(block, 1, Type::I32);
            let ext_x = make_op(&mut dfg, Opcode::Uextend, vec![x]);
            let ext_y = make_op(&mut dfg, Opcode::Uextend, vec![y]);
            let eq = make_op(&mut dfg, Opcode::Eq, vec![ext_x, ext_y]);
            assert!(rewrites_to_opcode(&mut dfg, eq, Opcode::Eq),
                    "eq(uextend(x), uextend(y)) should simplify");
            println!("  eq(uextend(x), uextend(y)) => eq(x, y)");
        }

        // 8. ineg(sextend(x)) => sextend(ineg(x))
        {
            let mut dfg = DataFlowGraph::new();
            let block = BlockId(0);
            let x = dfg.make_block_param(block, 0, Type::I32);
            let ext = make_op(&mut dfg, Opcode::Sextend, vec![x]);
            let neg = make_op(&mut dfg, Opcode::Ineg, vec![ext]);
            assert!(rewrites_to_opcode(&mut dfg, neg, Opcode::Sextend),
                    "ineg(sextend(x)) should push under extend");
            println!("  ineg(sextend(x)) => sextend(ineg(x))");
        }

        println!("  All extend/reduce ISLE rule tests passed!");
    }
}
