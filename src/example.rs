//! Complete example demonstrating the greenfield egraph pass
//!
//! This shows how to parse CLIF IR, run the egraph pass,
//! and see the optimization in action.

use crate::clif_parser::*;
use crate::egraph_pass::*;
use crate::elaborate::*;
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

    println!("{}", "=".repeat(70));
}

/// Example 1: Simple algebraic optimization
///
/// Demonstrates:
///   (x + 0) => x
///   (x * 1) => x
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
    let clif_input = r#"
function %algebraic_simplify(i32) -> i32 {
block0(v0: i32):
    ; y = x + 0 (should simplify to x)
    v1 = iconst.i32 0
    v2 = iadd.i32 v0, v1
    ; z = y * 1 (should simplify to y, which is x)
    v3 = iconst.i32 1
    v4 = imul.i32 v2, v3
    return v4
}
"#;

    println!("Example 1: Algebraic Simplification");

    let (dfg, layout) = parse_clif(clif_input).expect("Parse failed");
    let domtree = DominatorTree::from_linear_blocks(&layout.blocks);
    let mut pass = EgraphPass::new(dfg, layout, domtree);
    pass.run();

    print_optimization_result(
        "Example 1",
        clif_input,
        &pass.dfg,
        &pass.layout,
        "algebraic_simplify",
        &[Type::I32],
        Some(Type::I32),
    );
}

/// Example 2: GVN (Global Value Numbering)
///
/// Demonstrates deduplication of identical expressions.
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
    let clif_input = r#"
function %gvn_test(i32, i32) -> i32 {
block0(v0: i32, v1: i32):
    ; a = x + y
    v2 = iadd.i32 v0, v1
    ; b = x + y (duplicate - should be merged with a)
    v3 = iadd.i32 v0, v1
    ; c = a + b (after GVN: c = a + a)
    v4 = iadd.i32 v2, v3
    return v4
}
"#;

    println!("Example 2: Global Value Numbering (GVN)");

    let (dfg, layout) = parse_clif(clif_input).expect("Parse failed");
    let domtree = DominatorTree::from_linear_blocks(&layout.blocks);
    let mut pass = EgraphPass::new(dfg, layout, domtree);
    pass.run();

    print_optimization_result(
        "Example 2",
        clif_input,
        &pass.dfg,
        &pass.layout,
        "gvn_test",
        &[Type::I32, Type::I32],
        Some(Type::I32),
    );
}

/// Example 3: Union nodes and cost-based extraction
///
/// Shows how the egraph represents multiple equivalent forms
/// and chooses the best one based on cost.
///
/// x + x (cost 1) vs x * 2 (cost 2)
/// The elaborator should choose the cheaper form.
pub fn example_union_and_extraction() {
    let clif_input = r#"
function %cost_extraction(i32) -> i32 {
block0(v0: i32):
    ; x + x (cost 1 - addition is cheaper)
    v1 = iadd.i32 v0, v0
    ; x * 2 (cost 2 - multiplication is more expensive)
    v2 = iconst.i32 2
    v3 = imul.i32 v0, v2
    ; Return the first form; rewrite rules will create union
    ; and elaborator will pick the cheaper equivalent
    return v1
}
"#;

    println!("Example 3: Union Nodes and Cost-Based Extraction");

    let (dfg, layout) = parse_clif(clif_input).expect("Parse failed");
    let domtree = DominatorTree::from_linear_blocks(&layout.blocks);

    // First run the pass to build the egraph
    let mut pass = EgraphPass::new(dfg, layout, domtree);
    pass.run();

    print_optimization_result(
        "Cost-Based Extraction Result",
        clif_input,
        &pass.dfg,
        &pass.layout,
        "cost_extraction",
        &[Type::I32],
        Some(Type::I32),
    );
}

/// Example 4: Scoped GVN (dominance-aware)
///
/// Shows how GVN respects dominance through scoped hash maps.
/// Values computed in dominating blocks can be reused in dominated blocks.
///
/// block0:
///   a = x + y
///   jump block1
/// block1:
///   b = x + y  // can reuse 'a' since block0 dominates block1
///   return b
pub fn example_scoped_gvn() {
    let clif_input = r#"
function %scoped_gvn(i32, i32) -> i32 {
block0(v0: i32, v1: i32):
    ; a = x + y (computed in entry block)
    v2 = iadd.i32 v0, v1
    jump block1

block1():
    ; b = x + y (should merge with v2 from dominating block0)
    v3 = iadd.i32 v0, v1
    return v3
}
"#;

    println!("Example 4: Scoped GVN (Dominance-Aware)");

    let (dfg, layout) = parse_clif(clif_input).expect("Parse failed");
    let domtree = DominatorTree::from_linear_blocks(&layout.blocks);
    let mut pass = EgraphPass::new(dfg, layout, domtree);
    pass.run();

    print_optimization_result(
        "Scoped GVN Result",
        clif_input,
        &pass.dfg,
        &pass.layout,
        "scoped_gvn",
        &[Type::I32, Type::I32],
        Some(Type::I32),
    );
}

/// Example 5: Strength Reduction
///
/// Demonstrates strength reduction where expensive operations
/// are replaced with cheaper equivalents.
///
/// x * 2 => x + x (mul costs 2, add costs 1)
pub fn example_strength_reduction() {
    let clif_input = r#"
function %strength_reduce(i32) -> i32 {
block0(v0: i32):
    ; x * 2 can be strength-reduced to x + x
    v1 = iconst.i32 2
    v2 = imul.i32 v0, v1
    ; Use the result
    v3 = iadd.i32 v2, v2
    return v3
}
"#;

    println!("Example 5: Strength Reduction");

    let (dfg, layout) = parse_clif(clif_input).expect("Parse failed");
    let domtree = DominatorTree::from_linear_blocks(&layout.blocks);
    let mut pass = EgraphPass::new(dfg, layout, domtree);
    pass.run();

    print_optimization_result(
        "Strength Reduction Result",
        clif_input,
        &pass.dfg,
        &pass.layout,
        "strength_reduce",
        &[Type::I32],
        Some(Type::I32),
    );
}

/// Example 6: Conditional Branch Optimization
///
/// Shows how self-comparisons can be optimized.
/// x == x is always true.
pub fn example_conditional_optimization() {
    let clif_input = r#"
function %self_compare(i32) -> i32 {
block0(v0: i32):
    ; x == x is always true
    v1 = icmp.eq.i32 v0, v0
    brif v1, block1, block2

block1():
    ; This branch is always taken
    v2 = iconst.i32 42
    return v2

block2():
    ; This branch is never taken (dead code)
    v3 = iconst.i32 99
    return v3
}
"#;

    println!("Example 6: Conditional Branch Optimization");

    let (dfg, layout) = parse_clif(clif_input).expect("Parse failed");
    let domtree = DominatorTree::from_linear_blocks(&layout.blocks);
    let mut pass = EgraphPass::new(dfg, layout, domtree);
    pass.run();

    print_optimization_result(
        "Self-Comparison Result",
        clif_input,
        &pass.dfg,
        &pass.layout,
        "self_compare",
        &[Type::I32],
        Some(Type::I32),
    );
}

/// Example 7: Range-Aware Egraph Optimization
pub fn example_range_egraph_integration() {
    let clif_input = r#"
function %range_egraph(i32) -> i32 {
block0(v0: i32):
    ; Compare x < 10
    v1 = iconst.i32 10
    v2 = icmp.slt.i32 v0, v1
    brif v2, block1, block2

block1():
    ; In this block we know x < 10
    ; Should fold to 1
    v3 = iconst.i32 100
    v4 = icmp.slt.i32 v0, v3
    return v4

block2():
    ; Fallthrough: x >= 10
    v7 = iconst.i32 0
    return v7
}
"#;

    println!("Example 7: Range-Aware Egraph Optimization");

    let (dfg, layout) = parse_clif(clif_input).expect("Parse failed");
    let domtree = DominatorTree::from_linear_blocks(&layout.blocks);
    let mut pass = EgraphPass::new(dfg, layout, domtree);
    pass.run();

    print_optimization_result(
        "Range-Egraph Integration",
        clif_input,
        &pass.dfg,
        &pass.layout,
        "range_egraph",
        &[Type::I32],
        Some(Type::I32),
    );

    // Print range-specific stats
    println!("Range integration stats:");
    println!("  Ranges propagated: {}", pass.stats.ranges_propagated);
    println!(
        "  Refinement iterations: {}",
        pass.stats.range_refinement_iterations
    );
    println!(
        "  New unions from refinement: {}",
        pass.stats.range_refinement_new_unions
    );
}

/// Run all examples
pub fn run_all_examples() {
    println!("\n");
    println!("Demo");
    println!("\n");

    example_algebraic_simplification();
    println!("\n{}\n", "─".repeat(70));

    example_gvn();
    println!("\n{}\n", "─".repeat(70));

    example_union_and_extraction();
    println!("\n{}\n", "─".repeat(70));

    example_scoped_gvn();
    println!("\n{}\n", "─".repeat(70));

    example_strength_reduction();
    println!("\n{}\n", "─".repeat(70));

    example_conditional_optimization();
    println!("\n{}\n", "─".repeat(70));

    example_range_egraph_integration();

    println!("\n");
    println!("All examples completed.");
    println!("\n");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_all_examples() {
        // Test that all examples parse and run without panicking
        run_all_examples();
    }

    #[test]
    fn test_algebraic_simplification_clif_parse() {
        let clif_input = r#"
function %test(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 0
    v2 = iadd.i32 v0, v1
    return v2
}
"#;
        let result = parse_clif(clif_input);
        assert!(result.is_ok(), "CLIF parsing should succeed");
    }

    #[test]
    fn test_gvn_clif_parse() {
        let clif_input = r#"
function %test(i32, i32) -> i32 {
block0(v0: i32, v1: i32):
    v2 = iadd.i32 v0, v1
    v3 = iadd.i32 v0, v1
    v4 = iadd.i32 v2, v3
    return v4
}
"#;
        let result = parse_clif(clif_input);
        assert!(result.is_ok(), "CLIF parsing should succeed");
    }

    #[test]
    fn test_multi_block_clif_parse() {
        let clif_input = r#"
function %test(i32, i32) -> i32 {
block0(v0: i32, v1: i32):
    v2 = iadd.i32 v0, v1
    jump block1

block1():
    v3 = iadd.i32 v0, v1
    return v3
}
"#;
        let result = parse_clif(clif_input);
        assert!(result.is_ok(), "Multi-block CLIF parsing should succeed");
    }
}
