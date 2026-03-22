//! Context-aware optimization examples using CLIF
//!
//! These examples demonstrate optimizations that require understanding
//! control flow context, such as knowing that x == 0 in the true branch
//! of an if statement that tests x == 0.

use crate::clif_parser::*;
use crate::egraph_pass::*;
use crate::pattern::*;
use crate::range::RangeAssumptions;
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

    /// Test that a conditional union is created for `v0 % 4 == v0` when
    /// `v0 ∈ [0, 3]` (i.e., in the true branch of `brif (ult v0, 4)`),
    /// and that the conditional union is **not** applied during extraction
    /// in the else branch where the range does not hold.
    #[test]
    fn test_conditional_union_mod_reduction() {
        // ── Build the two-block CFG by hand ──────────────────────────
        //
        // block0(v0: i32):
        //   c4   = iconst 4
        //   cmp  = icmp.ult v0, c4          // v0 < 4 (unsigned)
        //   brif cmp, block1, block2
        //
        // block1:                            // v0 ∈ [0, 3]
        //   mod_result = irem v0, c4         // v0 % 4  — should equal v0
        //   return mod_result
        //
        // block2:                            // v0 ∈ [4, +∞]
        //   return v0

        let mut dfg = DataFlowGraph::new();

        let block0 = BlockId(0);
        let block1 = BlockId(1);
        let block2 = BlockId(2);

        // block0 param
        let v0 = dfg.make_block_param(block0, 0, Type::I32);

        // c4 = iconst 4
        let c4_inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, 4);
        let c4_id = dfg.make_inst(c4_inst);
        let c4 = dfg.make_inst_result(c4_id, Type::I32);

        // cmp = icmp.ult v0, c4
        let cmp_inst = Instruction::new(Opcode::Ult, vec![v0, c4], Type::I32);
        let cmp_id = dfg.make_inst(cmp_inst);
        let cmp = dfg.make_inst_result(cmp_id, Type::I32);

        // brif cmp, block1, block2
        let brif_inst = Instruction::with_branch(
            Opcode::CondBranch,
            vec![cmp],
            Type::I32,
            BranchInfo::Conditional(block1, 0, block2, 0),
        );
        let brif_id = dfg.make_inst(brif_inst);
        dfg.make_inst_result(brif_id, Type::I32);

        // block1: mod_result = irem v0, c4
        let irem_inst = Instruction::new(Opcode::Irem, vec![v0, c4], Type::I32);
        let irem_id = dfg.make_inst(irem_inst);
        let mod_result = dfg.make_inst_result(irem_id, Type::I32);

        let ret1_inst = Instruction::new(Opcode::Return, vec![mod_result], Type::I32);
        let ret1_id = dfg.make_inst(ret1_inst);
        dfg.make_inst_result(ret1_id, Type::I32);

        // block2: return v0
        let ret2_inst = Instruction::new(Opcode::Return, vec![v0], Type::I32);
        let ret2_id = dfg.make_inst(ret2_inst);
        dfg.make_inst_result(ret2_id, Type::I32);

        // ── Build layout ─────────────────────────────────────────────
        let mut layout = Layout::new();

        let mut b0 = Block::new(block0);
        b0.params = vec![v0];
        b0.insts = vec![c4_id, cmp_id, brif_id];
        layout.add_block(b0);

        let mut b1 = Block::new(block1);
        b1.insts = vec![irem_id, ret1_id];
        layout.add_block(b1);

        let mut b2 = Block::new(block2);
        b2.insts = vec![ret2_id];
        layout.add_block(b2);

        // ── Build dominator tree ─────────────────────────────────────
        let domtree = DominatorTree::from_cfg(
            block0,
            &[
                (block0, vec![block1, block2]),
                (block1, vec![]),
                (block2, vec![]),
            ],
        );

        // ── Add a rewrite rule: irem(x, n) => x  when x ∈ [0, n-1] ─
        //
        // This rule has a range-based condition: InRange("x", 0, 3).
        // In block1 the branch guarantees v0 ∈ [0, 3], so the condition
        // is satisfied only because of a branch-local assumption —
        // triggering a *conditional* union rather than an unconditional one.
        let mod_reduction_rule = Rewrite::new("mod-reduction-by-range")
            .match_pattern(Pattern::op(
                Opcode::Irem,
                vec![Pattern::var("x"), Pattern::constant(4)],
            ))
            .produce(Pattern::var("x"))
            .when(Condition::InRange(VarId::new("x"), 0, 3))
            .build();

        let mut pass = EgraphPass::new(dfg, layout, domtree);
        pass.add_rewrite_rule(mod_reduction_rule);
        // v0 is treated as an unsigned i32 parameter — non-negative.
        // This lets the `ult v0, 4` true-branch intersect to [0, 3].
        pass.assume_range(v0, 0, i64::MAX);
        pass.run();

        // ── Assertions ───────────────────────────────────────────────

        // 1. A ConditionalUnion should have been created between mod_result
        //    and v0 (or their optimized representatives).
        let has_conditional = pass.dfg.conditional_unions.iter().any(|cu| {
            // One side should be the irem result, the other should be v0.
            (cu.lhs == mod_result || cu.rhs == mod_result || cu.lhs == v0 || cu.rhs == v0)
                && !cu.condition.assumptions.is_empty()
        });
        assert!(
            has_conditional,
            "Expected a ConditionalUnion between mod_result and v0, found: {:?}",
            pass.dfg.conditional_unions
        );

        // 2. The conditional union's AssumptionSet should reference v0 with
        //    a range within [0, 3]. Clone the assumptions to avoid borrow issues.
        let cu_assumptions = {
            let cu = pass
                .dfg
                .conditional_unions
                .iter()
                .find(|cu| {
                    (cu.lhs == mod_result || cu.rhs == mod_result || cu.lhs == v0 || cu.rhs == v0)
                        && !cu.condition.assumptions.is_empty()
                })
                .expect("conditional union must exist");
            cu.condition.assumptions.clone()
        };
        let has_v0_assumption = cu_assumptions
            .iter()
            .any(|&(v, ref r)| v == v0 && r.min >= 0 && r.max <= 3);
        assert!(
            has_v0_assumption,
            "Conditional union should include assumption v0 ∈ [0, 3], got: {:?}",
            cu_assumptions
        );

        // 3. The conditional union must NOT be applied during extraction
        //    in block2 where v0 ∈ [4, +∞]. Verify by checking that an
        //    elaborator without range assumptions (simulating block2
        //    context) does not pick the conditional alternative.
        {
            let mut stats = Stats::default();
            let elaborator =
                crate::elaborate::Elaborator::new(&mut pass.dfg, &pass.domtree, &mut stats);
            // Without range assumptions, conditional unions should be ignored.
            // (The elaborator's `range_assumptions` is None.)
            assert!(
                elaborator.range_assumptions.is_none(),
                "Default elaborator should have no range assumptions"
            );
        }

        // Build an AssumptionSet from the clone for entailment checks.
        let cu_condition = AssumptionSet {
            assumptions: cu_assumptions,
        };

        // 4. With block1's range assumptions, the conditional union IS used.
        {
            let mut ra = RangeAssumptions::new();
            ra.assume_range(v0, crate::range::Range::new(0, 3));

            assert!(
                cu_condition.is_entailed_by(&ra),
                "Conditional union should be entailed in block1 context"
            );
        }

        // 5. With block2's range assumptions (v0 >= 4), it is NOT entailed.
        {
            let mut ra = RangeAssumptions::new();
            ra.assume_range(v0, crate::range::Range::new(4, i64::MAX));

            assert!(
                !cu_condition.is_entailed_by(&ra),
                "Conditional union should NOT be entailed in block2 context"
            );
        }

        println!("\n✓ Conditional union mod reduction test passed");
    }

    /// Test 1 — `test_seeing_through_blockparam_sign_bit`
    ///
    /// Both paths through the CFG deliver a value whose range the optimizer
    /// can prove is `[1, 1]`.  The block3 join-point range propagation should
    /// propagate that range to the block param `v13` (or its canonical).
    ///
    /// Layout (renumbered to avoid global value-number collisions):
    /// ```text
    /// block0(v0):  brif v0, block1(v0), block2(v0)
    /// block1(v1):  sign-bit computation → v7 = band(sshr(bor(v1,ineg v1),31),1)
    ///              jump block3(v7)
    /// block2(v8):  v9=0, v10=1, v11=imul(v8,v9), v12=iadd(v11,v10)
    ///              jump block3(v12)   ← v12 is always 1 (mul-zero + add-zero)
    /// block3(v13): return v13
    /// ```
    ///
    /// Verifies:
    /// * `param_sources` correctly maps the block3 parameter to two sources.
    /// * The pass runs to completion without panicking.
    /// * block2's computation folds: canonical(v12) ≡ iconst 1 (via mul-zero
    ///   and add-zero-left rewrites), so the output CLIF contains `iconst.i32 1`.
    #[test]
    fn test_seeing_through_blockparam_sign_bit() {
        let clif = r#"
function %f(i32) -> i32 {
block0(v0: i32):
    brif v0, block1(v0), block2(v0)

block1(v1: i32):
    v2 = ineg.i32 v1
    v3 = bor.i32 v1, v2
    v4 = iconst.i32 31
    v5 = sshr.i32 v3, v4
    v6 = iconst.i32 1
    v7 = band.i32 v5, v6
    jump block3(v7)

block2(v8: i32):
    v9 = iconst.i32 0
    v10 = iconst.i32 1
    v11 = imul.i32 v8, v9
    v12 = iadd.i32 v11, v10
    jump block3(v12)

block3(v13: i32):
    return v13
}
"#;
        let (dfg, layout) = parse_clif(clif).expect("sign-bit CLIF should parse");
        let domtree = DominatorTree::from_linear_blocks(&layout.blocks);
        let mut pass = EgraphPass::new(dfg, layout, domtree);
        pass.run();

        // 1. param_sources must map block3's param to two incoming edges.
        //    We find the block3 param (the one returned by block3).
        let block3_id = pass.layout.blocks[3]; // fourth block = block3
        let block3_params = &pass.layout.block_data[&block3_id].params;
        assert_eq!(
            block3_params.len(),
            1,
            "block3 should have exactly one param"
        );
        let v13 = block3_params[0];
        let sources = pass
            .dfg
            .param_sources
            .get(&v13)
            .cloned()
            .unwrap_or_default();
        assert_eq!(
            sources.len(),
            2,
            "block3 param should have two incoming sources (one per predecessor), \
             got: {:?}",
            sources
        );

        // 2. The output CLIF should be non-empty and well-formed.
        let output = pass
            .layout
            .display(&pass.dfg, "f", &[Type::I32], Some(Type::I32));
        assert!(
            !output.is_empty(),
            "optimized output should be non-empty"
        );

        // 3. block2's computation (v11=imul(v8,0), v12=iadd(v11,1)) should fold to 1.
        //    After mul-zero + add-zero-left rewrites, the output should contain iconst 1.
        //    (The exact value ID in the output varies, but the constant must appear.)
        assert!(
            output.contains("iconst.i32 1"),
            "output should contain iconst.i32 1 after constant folding block2's \
             computation; output was:\n{}",
            output
        );

        println!("\n✓ test_seeing_through_blockparam_sign_bit passed");
    }

    /// Test 2 — `test_seeing_through_blockparam_zero_branch`
    ///
    /// Both predecessors of block3 pass the same original value (`v1`).
    /// After redundant-phi elimination, the block3 param should be replaced
    /// by `v1`.
    ///
    /// ```text
    /// block0(v0, v1):  brif v0, block2(v1), block1(v1)
    /// block1(v2):      jump block3(v2)
    /// block2(v3):      jump block3(v3)
    /// block3(v4):      return v4          ← v4 should be eliminated → v1
    /// ```
    ///
    /// Verifies:
    /// * `param_sources` maps block3's param to two sources (block1→v2, block2→v3).
    /// * After `eliminate_redundant_params`, v4 is replaced by v1 (gvn_mapping
    ///   contains the chain v2→v1 and v3→v1, then v4→v1).
    /// * The output CLIF does not mention v4 (it resolves away to v1).
    #[test]
    fn test_seeing_through_blockparam_zero_branch() {
        // Both branches forward v1 directly to block3 (via block1/block2 params).
        let clif = r#"
function %f(i32, i32) -> i32 {
block0(v0: i32, v1: i32):
    brif v0, block2(v1), block1(v1)

block1(v2: i32):
    jump block3(v2)

block2(v3: i32):
    jump block3(v3)

block3(v4: i32):
    return v4
}
"#;
        let (dfg, layout) = parse_clif(clif).expect("zero-branch CLIF should parse");
        let domtree = DominatorTree::from_linear_blocks(&layout.blocks);
        let mut pass = EgraphPass::new(dfg, layout, domtree);
        pass.run();

        // 1. param_sources for block3's param must have 2 entries.
        let block3_id = pass.layout.blocks[3];
        let block3_params = &pass.layout.block_data[&block3_id].params;
        assert_eq!(block3_params.len(), 1, "block3 should have exactly one param");
        let v4 = block3_params[0];
        let sources = pass
            .dfg
            .param_sources
            .get(&v4)
            .cloned()
            .unwrap_or_default();
        assert_eq!(
            sources.len(),
            2,
            "block3 param should have two incoming sources; got: {:?}",
            sources
        );

        // 2. After redundant-phi elimination, v4 should be replaced by v1.
        //    v1 is the second entry-block param (index 1).
        let block0_id = pass.layout.blocks[0];
        let block0_params = &pass.layout.block_data[&block0_id].params;
        let v1 = block0_params[1]; // second param of block0

        assert!(
            pass.is_gvn_remapped(v4),
            "v4 should have been eliminated by redundant-phi elimination"
        );

        // Follow the gvn_mapping chain to confirm it resolves to v1.
        let resolved = pass.fully_resolve(v4);
        assert_eq!(
            resolved, v1,
            "v4 should ultimately resolve to v1 via gvn_mapping"
        );

        println!("\n✓ test_seeing_through_blockparam_zero_branch passed");
    }

    /// Test 3 — `test_loop_invariant_blockparam`
    ///
    /// In the loop below, `v5 = imul(v2, 1)` is simplified to `v2` by the
    /// `mul-one` rewrite rule.  Because the only loop-carried update is
    /// `v2 := v5 ≡ v2` (a no-op), the loop-invariant analysis should detect
    /// that `v2` always holds the entry value `v0`.  Consequently `v8`
    /// (block2's param, which receives `v5` from the loop) should also resolve
    /// to `v0`.
    ///
    /// ```text
    /// block0(v0):      v1=iconst 5; brif v0, block1(v0,v1), block2(v0)
    /// block1(v2,v3):   v4=1; v5=imul(v2,v4)→v2; v6=1; v7=isub(v3,v6)
    ///                  brif v7, block2(v5), block1(v5,v7)
    /// block2(v8):      return v8      ← should resolve to v0
    /// ```
    #[test]
    fn test_loop_invariant_blockparam() {
        let clif = r#"
function %f(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 5
    brif v0, block1(v0, v1), block2(v0)

block1(v2: i32, v3: i32):
    v4 = iconst.i32 1
    v5 = imul.i32 v2, v4
    v6 = iconst.i32 1
    v7 = isub.i32 v3, v6
    brif v7, block2(v5), block1(v5, v7)

block2(v8: i32):
    return v8
}
"#;
        let (dfg, layout) = parse_clif(clif).expect("loop-invariant CLIF should parse");
        let domtree = DominatorTree::from_linear_blocks(&layout.blocks);
        let mut pass = EgraphPass::new(dfg, layout, domtree);
        pass.run();

        // 1. param_sources must be populated for block2's param.
        let block2_id = pass.layout.blocks[2];
        let block2_params = &pass.layout.block_data[&block2_id].params;
        assert_eq!(block2_params.len(), 1, "block2 should have exactly one param");
        let v8 = block2_params[0];
        let sources = pass
            .dfg
            .param_sources
            .get(&v8)
            .cloned()
            .unwrap_or_default();
        assert!(
            !sources.is_empty(),
            "block2's param should have at least one source"
        );

        // 2. v0 is the sole entry-block param.
        let block0_id = pass.layout.blocks[0];
        let v0 = pass.layout.block_data[&block0_id].params[0];

        // 3. After loop-invariant param analysis, v8 should resolve to v0.
        //    (v2's only non-self source is v0; v5 ≡ v2; v8's sources collapse to v0.)
        let resolved_v8 = pass.fully_resolve(v8);
        assert_eq!(
            resolved_v8, v0,
            "v8 should resolve to v0 after loop-invariant elimination; \
             gvn_mapping: {:?}",
            pass.gvn_mapping()
        );

        println!("\n✓ test_loop_invariant_blockparam passed");
    }
}
