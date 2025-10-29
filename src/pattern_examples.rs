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
}
