// Complete example demonstrating the acyclic e-graph in action

use crate::aegraph_core::{AEGraph, Id, Node};
use crate::aegraph_extract::{Extractor, OpCountCost, Term};
use crate::aegraph_rules::AEGraphWithRules;

/// Example 1: Basic optimization
fn example_basic_optimization() {
    println!("=== Example 1: Basic Optimization ===\n");

    let mut g = AEGraphWithRules::new();

    // Build expression: (x + 0) * 1
    println!("Building expression: (x + 0) * 1");
    let x = g.add(Node::new("var", vec![]));
    let zero = g.add(Node::with_data("const", vec![], 0));
    let one = g.add(Node::with_data("const", vec![], 1));

    let x_plus_0 = g.add(Node::new("add", vec![x, zero]));
    let result = g.add(Node::new("mul", vec![x_plus_0, one]));

    println!("Original eclass: {:?}", result);
    println!("Canonical eclass: {:?}", g.find(result));
    println!("x eclass: {:?}", g.find(x));

    // After optimization, should equal x
    if g.find(result) == g.find(x) {
        println!("✓ Successfully optimized to x!\n");
    } else {
        println!("Note: Rules applied, extracting best term...\n");
    }

    // Extract the optimized expression
    let mut extractor = Extractor::new(&mut g.graph, OpCountCost);
    let term = extractor.build_term(result);

    println!("Extracted term: {}", term.to_string());
    println!();
}

/// Example 2: Complex expression with multiple optimizations
fn example_complex_expression() {
    println!("=== Example 2: Complex Expression ===\n");

    let mut g = AEGraphWithRules::new();

    // Build: ((x * 1) + (y * 0)) + (0 + z)
    println!("Building: ((x * 1) + (y * 0)) + (0 + z)");

    let x = g.add(Node::new("var", vec![]));
    let y = g.add(Node::new("var", vec![]));
    let z = g.add(Node::new("var", vec![]));
    let zero = g.add(Node::with_data("const", vec![], 0));
    let one = g.add(Node::with_data("const", vec![], 1));

    let x_times_1 = g.add(Node::new("mul", vec![x, one]));
    let y_times_0 = g.add(Node::new("mul", vec![y, zero]));
    let left = g.add(Node::new("add", vec![x_times_1, y_times_0]));

    let zero_plus_z = g.add(Node::new("add", vec![zero, z]));
    let result = g.add(Node::new("add", vec![left, zero_plus_z]));

    // Extract and display
    let mut extractor = Extractor::new(&mut g.graph, OpCountCost);
    let term = extractor.build_term(result);

    println!("Optimized to: {}", term.to_string());
    println!("Expected: (x + z) or similar simplification\n");
}

/// Example 3: Hash-consing demonstration
fn example_hash_consing() {
    println!("=== Example 3: Hash-Consing ===\n");

    let mut g = AEGraphWithRules::new();

    let x = g.add(Node::new("var", vec![]));

    // Create the same expression twice
    println!("Creating x + x twice...");
    let expr1 = g.add(Node::new("add", vec![x, x]));
    let expr2 = g.add(Node::new("add", vec![x, x]));

    if expr1 == expr2 {
        println!("✓ Hash-consing working: both expressions have same ID");
        println!("  expr1: {:?}", expr1);
        println!("  expr2: {:?}", expr2);
    } else {
        println!("✗ Different IDs (shouldn't happen)");
    }

    println!("Total eclasses created: {}\n", g.graph.num_eclasses());
}

/// Example 4: Manual union demonstration
fn example_manual_union() {
    println!("=== Example 4: Manual Union ===\n");

    let mut g = AEGraph::new();

    // Create two different but semantically equivalent expressions
    let x = g.add(Node::new("var", vec![]));
    let y = g.add(Node::new("var", vec![]));

    println!("Creating x + y and y + x...");
    let expr1 = g.add(Node::new("add", vec![x, y]));
    let expr2 = g.add(Node::new("add", vec![y, x]));

    println!("Before union:");
    println!("  x + y: {:?}", expr1);
    println!("  y + x: {:?}", expr2);
    println!("  Same? {}", expr1 == expr2);

    // Manually union them (commutativity)
    println!("\nApplying commutativity: union(x+y, y+x)");
    g.union(expr1, expr2);

    println!("\nAfter union:");
    println!("  x + y canonical: {:?}", g.find(expr1));
    println!("  y + x canonical: {:?}", g.find(expr2));
    println!("  Same? {}", g.find(expr1) == g.find(expr2));
    println!();
}

/// Example 5: Cost-based extraction
fn example_cost_based_extraction() {
    println!("=== Example 5: Cost-Based Extraction ===\n");

    let mut g = AEGraph::new();

    let x = g.add(Node::new("var", vec![]));
    let two = g.add(Node::with_data("const", vec![], 2));

    // Create: x + x (cost 1)
    let expr1 = g.add(Node::new("add", vec![x, x]));

    // Create: x * 2 (cost 2, more expensive)
    let expr2 = g.add(Node::new("mul", vec![x, two]));

    // Union them (they're semantically equivalent)
    println!("Creating two equivalent expressions:");
    println!("  x + x (cost ~1)");
    println!("  x * 2 (cost ~2)");
    g.union(expr1, expr2);

    // Extract - should prefer addition
    let mut extractor = Extractor::new(&mut g, OpCountCost);
    let (cost, node) = extractor.extract(expr1);
    let term = extractor.build_term(expr1);

    println!("\nExtractor chose:");
    println!("  Cost: {:?}", cost);
    println!("  Operation: {}", node.op);
    println!("  Full term: {}", term.to_string());
    println!();
}

/// Example 6: Demonstrating acyclicity
fn example_acyclicity() {
    println!("=== Example 6: Acyclicity Property ===\n");

    let mut g = AEGraph::new();

    // Create a chain of dependencies
    let a = g.add(Node::with_data("const", vec![], 1));
    let b = g.add(Node::new("add", vec![a, a])); // b depends on a
    let c = g.add(Node::new("mul", vec![b, b])); // c depends on b
    let d = g.add(Node::new("add", vec![c, a])); // d depends on c and a

    println!("Created chain: a -> b -> c -> d");
    println!("  a (const 1): {:?}", a);
    println!("  b (a + a): {:?}", b);
    println!("  c (b * b): {:?}", c);
    println!("  d (c + a): {:?}", d);

    println!("\nKey property: Each ID only refers to earlier IDs");
    println!("This ensures the graph is acyclic!\n");

    // Extract to show the tree structure
    let mut extractor = Extractor::new(&mut g, OpCountCost);
    let term = extractor.build_term(d);
    println!("Expression tree: {}", term.to_string());
    println!();
}

/// Main example runner
pub fn run_all_examples() {
    println!("\n╔═══════════════════════════════════════════╗");
    println!("║  Acyclic E-graph Examples                 ║");
    println!("╚═══════════════════════════════════════════╝\n");

    example_basic_optimization();
    example_complex_expression();
    example_hash_consing();
    example_manual_union();
    example_cost_based_extraction();
    example_acyclicity();

    println!("╔═══════════════════════════════════════════╗");
    println!("║  All examples completed!                  ║");
    println!("╚═══════════════════════════════════════════╝\n");
}

/// Quick demo for testing
pub fn quick_demo() {
    println!("\n🚀 Quick Demo: Optimizing (x + 0) * 1\n");

    let mut g = AEGraphWithRules::new();

    let x = g.add(Node::new("var", vec![]));
    let zero = g.add(Node::with_data("const", vec![], 0));
    let one = g.add(Node::with_data("const", vec![], 1));
    let x_plus_0 = g.add(Node::new("add", vec![x, zero]));
    let result = g.add(Node::new("mul", vec![x_plus_0, one]));

    let mut extractor = Extractor::new(&mut g.graph, OpCountCost);
    let term = extractor.build_term(result);

    println!("Input:  (x + 0) * 1");
    println!("Output: {}", term.to_string());
    println!();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_all_examples() {
        // Run all examples as tests
        example_basic_optimization();
        example_complex_expression();
        example_hash_consing();
        example_manual_union();
        example_cost_based_extraction();
        example_acyclicity();
    }

    #[test]
    fn test_quick_demo() {
        quick_demo();
    }
}
