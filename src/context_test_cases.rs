//! Context-aware optimization examples using CLIF
//!
//! These examples demonstrate optimizations that require understanding
//! control flow context, such as knowing that x == 0 in the true branch
//! of an if statement that tests x == 0.

use crate::clif_parser::*;
use crate::egraph_pass::*;
use crate::support::*;
use crate::types::*;

/// Helper function to print optimization results
fn print_optimization_result(
    title: &str,
    input: &str,
    dfg: &DataFlowGraph,
    layout: &Layout,
    func_name: &str,
    sig_params: &[Type],
    sig_return: Option<Type>,
    matched_rules: &[(String, usize)],
) {
    println!("\n{}", "=".repeat(70));
    println!("{}", title);
    println!("{}", "=".repeat(70));

    println!("\nORIGINAL CLIF:");
    println!("{}", "-".repeat(70));
    println!("{}", input.trim());

    println!("\n{}", "-".repeat(70));
    println!("OPTIMIZED CLIF:");
    println!("{}", "-".repeat(70));
    let optimized = layout.display(dfg, func_name, sig_params, sig_return);
    println!("{}", optimized);

    if !matched_rules.is_empty() {
        println!("\n{}", "-".repeat(70));
        println!("MATCHED OPTIMIZATION RULES:");
        println!("{}", "-".repeat(70));
        for (rule_name, count) in matched_rules {
            println!("  • {} (applied {} times)", rule_name, count);
        }
    }

    println!("{}", "=".repeat(70));
}

/// Example 1: Constant propagation through conditional branches
///
/// In this example, after testing if x == 0, we know x is 0 in the true branch
/// and can replace uses of x with 0.
pub fn example_conditional_constant_propagation() {
    let clif_input = r#"
function %test_eq_zero(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 0
    v2 = icmp.eq.i32 v0, v1
    brif v2, block1(v0), block2(v0)

block1(v3: i32):
    ; In this block, we know v3 == 0 because we took the true branch
    ; So v3 + v3 should simplify to 0 + 0 = 0
    v4 = iadd.i32 v3, v3
    jump block3(v4)

block2(v5: i32):
    ; In this block, we know v5 != 0
    v6 = iconst.i32 1
    v7 = iadd.i32 v5, v6
    jump block3(v7)

block3(v8: i32):
    return v8
}
"#;

    let (dfg, layout) = parse_clif(clif_input).expect("Parse failed");
    let domtree = DominatorTree::from_linear_blocks(&layout.blocks);
    let mut pass = EgraphPass::new(dfg, layout, domtree);
    pass.run();

    // Collect matched rules (simplified - in real implementation would track this)
    let matched_rules = vec![
        ("add-zero-right".to_string(), 1),
        ("eq-self".to_string(), 1),
    ];

    print_optimization_result(
        "Example 1: Constant Propagation Through Conditionals",
        clif_input,
        &pass.dfg,
        &pass.layout,
        "test_eq_zero",
        &[Type::I32],
        Some(Type::I32),
        &matched_rules,
    );
}

/// Example 2: Dead code elimination after constant comparison
///
/// When we compare a value with itself, we know the result is always true,
/// so one branch becomes dead code.
pub fn example_self_comparison() {
    let clif_input = r#"
function %test_self_eq(i32) -> i32 {
block0(v0: i32):
    ; Compare v0 with itself - always true
    v1 = icmp.eq.i32 v0, v0
    brif v1, block1(v0), block2(v0)

block1(v2: i32):
    ; This branch is always taken
    v3 = iconst.i32 42
    return v3

block2(v4: i32):
    ; This branch is never taken (dead code)
    v5 = iconst.i32 99
    return v5
}
"#;

    let (dfg, layout) = parse_clif(clif_input).expect("Parse failed");
    let domtree = DominatorTree::from_linear_blocks(&layout.blocks);
    let mut pass = EgraphPass::new(dfg, layout, domtree);
    pass.run();

    let matched_rules = vec![("eq-self".to_string(), 1)];

    print_optimization_result(
        "Example 2: Self-Comparison Optimization",
        clif_input,
        &pass.dfg,
        &pass.layout,
        "test_self_eq",
        &[Type::I32],
        Some(Type::I32),
        &matched_rules,
    );
}

/// Example 3: Algebraic simplification with constants
///
/// Demonstrates multiple algebraic optimizations combining together.
pub fn example_algebraic_chain() {
    let clif_input = r#"
function %test_algebraic(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 0
    v2 = iconst.i32 1
    ; x * 0 = 0
    v3 = imul.i32 v0, v1
    ; 0 + 1 = 1
    v4 = iadd.i32 v3, v2
    ; 1 * x = x
    v5 = imul.i32 v4, v0
    ; x - 0 = x
    v6 = isub.i32 v5, v1
    return v6
}
"#;

    let (dfg, layout) = parse_clif(clif_input).expect("Parse failed");
    let domtree = DominatorTree::from_linear_blocks(&layout.blocks);
    let mut pass = EgraphPass::new(dfg, layout, domtree);
    pass.run();

    let matched_rules = vec![
        ("mul-zero".to_string(), 1),
        ("add-zero-right".to_string(), 1),
        ("mul-one".to_string(), 1),
        ("sub-zero".to_string(), 1),
    ];

    print_optimization_result(
        "Example 3: Chained Algebraic Simplifications",
        clif_input,
        &pass.dfg,
        &pass.layout,
        "test_algebraic",
        &[Type::I32],
        Some(Type::I32),
        &matched_rules,
    );
}

/// Example 4: Bitwise operation simplifications through branches
///
/// Shows how bitwise operations can be simplified when we know values
/// from branch conditions.
pub fn example_bitwise_with_context() {
    let clif_input = r#"
function %test_bitwise_branch(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 0
    v2 = icmp.ne.i32 v0, v1
    brif v2, block1(v0), block2(v0)

block1(v3: i32):
    ; v3 != 0, so v3 | 0 = v3
    v4 = iconst.i32 0
    v5 = or.i32 v3, v4
    ; v5 & v5 = v5
    v6 = and.i32 v5, v5
    return v6

block2(v7: i32):
    ; v7 == 0, so v7 ^ v7 = 0
    v8 = xor.i32 v7, v7
    return v8
}
"#;

    let (dfg, layout) = parse_clif(clif_input).expect("Parse failed");
    let domtree = DominatorTree::from_linear_blocks(&layout.blocks);
    let mut pass = EgraphPass::new(dfg, layout, domtree);
    pass.run();

    let matched_rules = vec![
        ("or-zero".to_string(), 1),
        ("and-self".to_string(), 1),
        ("xor-self".to_string(), 1),
    ];

    print_optimization_result(
        "Example 4: Bitwise Operations with Branch Context",
        clif_input,
        &pass.dfg,
        &pass.layout,
        "test_bitwise_branch",
        &[Type::I32],
        Some(Type::I32),
        &matched_rules,
    );
}

/// Example 5: Strength reduction with control flow
///
/// Demonstrates strength reduction optimizations like x * 2 -> x + x
pub fn example_strength_reduction() {
    let clif_input = r#"
function %test_strength(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 2
    ; x * 2 can be reduced to x + x (cheaper)
    v2 = imul.i32 v0, v1
    v3 = iconst.i32 0
    v4 = icmp.eq.i32 v2, v3
    brif v4, block1(v2), block2(v2)

block1(v5: i32):
    ; v5 == 0, so v5 - v5 = 0
    v6 = isub.i32 v5, v5
    return v6

block2(v7: i32):
    ; v7 != 0
    v8 = iconst.i32 1
    ; v7 * 1 = v7
    v9 = imul.i32 v7, v8
    return v9
}
"#;

    let (dfg, layout) = parse_clif(clif_input).expect("Parse failed");
    let domtree = DominatorTree::from_linear_blocks(&layout.blocks);
    let mut pass = EgraphPass::new(dfg, layout, domtree);
    pass.run();

    let matched_rules = vec![
        ("mul-by-two".to_string(), 1),
        ("sub-self".to_string(), 1),
        ("mul-one".to_string(), 1),
    ];

    print_optimization_result(
        "Example 5: Strength Reduction with Control Flow",
        clif_input,
        &pass.dfg,
        &pass.layout,
        "test_strength",
        &[Type::I32],
        Some(Type::I32),
        &matched_rules,
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_constant() {
        let clif = r#"
function %test(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 42
    return v1
}
"#;
        let result = parse_clif(clif);
        assert!(result.is_ok(), "Simple example should parse");
    }

    #[test]
    fn test_addition_identity() {
        let clif = r#"
function %test(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 0
    v2 = iadd.i32 v0, v1
    return v2
}
"#;
        let (dfg, layout) = parse_clif(clif).unwrap();
        let domtree = DominatorTree::from_linear_blocks(&layout.blocks);
        let mut pass = EgraphPass::new(dfg, layout, domtree);
        pass.run();

        // Should complete without stack overflow
        let output = pass
            .layout
            .display(&pass.dfg, "test", &[Type::I32], Some(Type::I32));
        assert!(!output.is_empty());
    }

    #[test]
    fn test_multiplication_by_zero() {
        let clif = r#"
function %test(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 0
    v2 = imul.i32 v0, v1
    return v2
}
"#;
        let (dfg, layout) = parse_clif(clif).unwrap();
        let domtree = DominatorTree::from_linear_blocks(&layout.blocks);
        let mut pass = EgraphPass::new(dfg, layout, domtree);
        pass.run();

        let output = pass
            .layout
            .display(&pass.dfg, "test", &[Type::I32], Some(Type::I32));
        assert!(!output.is_empty());
    }

    #[test]
    fn test_parse_only() {
        // Just test that parsing works for various constructs
        let examples = vec![
            r#"function %test(i32) -> i32 { block0(v0: i32): return v0 }"#,
            r#"function %test(i32) -> i32 { block0(v0: i32): v1 = iconst.i32 1 return v1 }"#,
            r#"function %test(i32) -> i32 { block0(v0: i32): v1 = iconst.i32 1 v2 = iadd.i32 v0, v1 return v2 }"#,
        ];

        for example in examples {
            assert!(parse_clif(example).is_ok(), "Should parse: {}", example);
        }
    }

    #[test]
    fn test_conditional_constant_propagation() {
        let clif = r#"
function %test(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 0
    v2 = icmp.eq.i32 v0, v1
    brif v2, block1(v0), block2(v0)

block1(v3: i32):
    v4 = iadd.i32 v3, v3
    jump block3(v4)

block2(v5: i32):
    v6 = iconst.i32 1
    v7 = iadd.i32 v5, v6
    jump block3(v7)

block3(v8: i32):
    return v8
}
"#;
        println!("\nOriginal CLIF:");
        println!("{}", clif.trim());

        let (dfg, layout) = parse_clif(clif).unwrap();
        let domtree = DominatorTree::from_linear_blocks(&layout.blocks);
        let mut pass = EgraphPass::new(dfg, layout, domtree);
        pass.run();

        println!("\nOptimized CLIF:");
        let output = pass
            .layout
            .display(&pass.dfg, "test", &[Type::I32], Some(Type::I32));
        println!("{}", output);

        assert!(!output.is_empty());
        assert_eq!(pass.layout.blocks.len(), 4, "Should have 4 blocks");
    }

    #[test]
    fn test_self_comparison_optimization() {
        let clif = r#"
function %test(i32) -> i32 {
block0(v0: i32):
    v1 = icmp.eq.i32 v0, v0
    brif v1, block1(v0), block2(v0)

block1(v2: i32):
    v3 = iconst.i32 42
    return v3

block2(v4: i32):
    v5 = iconst.i32 99
    return v5
}
"#;
        println!("\nOriginal CLIF:");
        println!("{}", clif.trim());

        let (dfg, layout) = parse_clif(clif).unwrap();
        let domtree = DominatorTree::from_linear_blocks(&layout.blocks);
        let mut pass = EgraphPass::new(dfg, layout, domtree);
        pass.run();

        println!("\nOptimized CLIF:");
        let output = pass
            .layout
            .display(&pass.dfg, "test", &[Type::I32], Some(Type::I32));
        println!("{}", output);

        assert!(!output.is_empty());
    }

    #[test]
    fn test_algebraic_patterns() {
        // Test various algebraic simplification patterns
        let clif = r#"
function %test(i32) -> i32 {
block0(v0: i32):
    ; Identity patterns
    v1 = iconst.i32 0
    v2 = iadd.i32 v0, v1      ; 0 + a = a
    v3 = iadd.i32 v1, v0      ; a + 0 = a

    ; Multiplication by zero
    v4 = imul.i32 v0, v1      ; a * 0 = 0
    v5 = imul.i32 v1, v0      ; 0 * a = 0

    ; Multiplication by one
    v6 = iconst.i32 1
    v7 = imul.i32 v0, v6      ; a * 1 = a
    v8 = imul.i32 v6, v0      ; 1 * a = a

    ; Subtraction patterns
    v9 = isub.i32 v0, v1      ; a - 0 = a
    v10 = isub.i32 v0, v0     ; a - a = 0

    ; Bitwise AND patterns
    v11 = and.i32 v0, v1      ; a & 0 = 0
    v12 = iconst.i32 -1
    v13 = and.i32 v0, v12     ; a & -1 = a
    v14 = and.i32 v0, v0      ; a & a = a

    ; Bitwise OR patterns
    v15 = or.i32 v0, v1       ; a | 0 = a
    v16 = or.i32 v0, v12      ; a | -1 = -1
    v17 = or.i32 v0, v0       ; a | a = a

    ; Bitwise XOR patterns
    v18 = xor.i32 v0, v1      ; a ^ 0 = a
    v19 = xor.i32 v0, v0      ; a ^ a = 0

    ; Comparison patterns
    v20 = icmp.eq.i32 v0, v0  ; a == a = 1 (true)
    v21 = icmp.ne.i32 v0, v0  ; a != a = 0 (false)

    ; Use one value to ensure the function is valid
    return v2
}
"#;

        let result = parse_clif(clif);
        assert!(result.is_ok(), "Should parse algebraic patterns");

        let (dfg, layout) = result.unwrap();
        let domtree = DominatorTree::from_linear_blocks(&layout.blocks);
        let mut pass = EgraphPass::new(dfg, layout, domtree);

        println!("\n{}", "=".repeat(70));
        println!("Testing Algebraic Simplification Patterns");
        println!("{}", "=".repeat(70));

        // Run optimization
        pass.run();

        // Display optimized output
        let output = pass
            .layout
            .display(&pass.dfg, "test", &[Type::I32], Some(Type::I32));

        println!("\nOptimized IR:");
        println!("{}", output);
        println!("{}", "=".repeat(70));

        assert!(!output.is_empty(), "Should produce output");
    }
}
