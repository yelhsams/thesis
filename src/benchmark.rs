//! Benchmarking harness for comparing path-sensitive vs baseline runs.

use crate::clif_parser::parse_clif;
use crate::egraph_pass::EgraphPass;
use crate::support::DominatorTree;
use crate::types::*;

pub struct TestCase {
    pub name: &'static str,
    pub clif: &'static str,
}

pub const PATH_SENSITIVE_TESTS: &[TestCase] = &[
    // --- Range-conditioned rewrites (sdiv → ushr) ---

    // Branch guards v0 >= 0, so sdiv(v0,2) → ushr(v0,1) in the true arm.
    // Without path sensitivity the range is unknown and sdiv stays.
    TestCase {
        name: "sdiv2_guarded_nonneg",
        clif: r#"
function %sdiv2_guarded_nonneg(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 0
    v2 = icmp.sge.i32 v0, v1
    brif v2, block1(v0), block2(v0)

block1(v3: i32):
    v4 = iconst.i32 2
    v5 = sdiv.i32 v3, v4
    return v5

block2(v6: i32):
    return v6
}
"#,
    },
    // Two nested guards: v0 >= 0 then v0 < 100.
    // Inner block can fold sdiv(v0,8) → ushr(v0,3) because range [0,99].
    TestCase {
        name: "sdiv8_nested_guard",
        clif: r#"
function %sdiv8_nested_guard(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 0
    v2 = icmp.sge.i32 v0, v1
    brif v2, block1(v0), block3(v0)

block1(v3: i32):
    v4 = iconst.i32 100
    v5 = icmp.slt.i32 v3, v4
    brif v5, block2(v3), block3(v3)

block2(v6: i32):
    v7 = iconst.i32 8
    v8 = sdiv.i32 v6, v7
    return v8

block3(v9: i32):
    return v9
}
"#,
    },
    // --- irem elimination via range ---

    // Guard v0 in [0,7] via ult < 8 && sge >= 0, then irem(v0, 16) == v0.
    TestCase {
        name: "irem_subsumed_by_range",
        clif: r#"
function %irem_subsumed_by_range(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 8
    v2 = icmp.ult.i32 v0, v1
    v3 = iconst.i32 0
    v4 = icmp.sge.i32 v0, v3
    v5 = band.i32 v2, v4
    brif v5, block1(v0), block2(v0)

block1(v6: i32):
    v7 = iconst.i32 16
    v8 = irem.i32 v6, v7
    return v8

block2(v9: i32):
    return v9
}
"#,
    },
    // --- Comparison folding via range ---

    // Guard v0 < 10 (unsigned) && v0 >= 0 (signed).  Inside, icmp.slt v0, 100
    // is provably true → fold to 1.
    TestCase {
        name: "cmp_fold_slt_true",
        clif: r#"
function %cmp_fold_slt_true(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 10
    v2 = icmp.ult.i32 v0, v1
    v3 = iconst.i32 0
    v4 = icmp.sge.i32 v0, v3
    v5 = band.i32 v2, v4
    brif v5, block1(v0), block2(v0)

block1(v6: i32):
    v7 = iconst.i32 100
    v8 = icmp.slt.i32 v6, v7
    return v8

block2(v9: i32):
    v10 = iconst.i32 0
    return v10
}
"#,
    },
    // Guard v0 >= 50.  Inside, icmp.slt v0, 10 is provably false → fold to 0.
    TestCase {
        name: "cmp_fold_slt_false",
        clif: r#"
function %cmp_fold_slt_false(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 50
    v2 = icmp.sge.i32 v0, v1
    brif v2, block1(v0), block2(v0)

block1(v3: i32):
    v4 = iconst.i32 10
    v5 = icmp.slt.i32 v3, v4
    return v5

block2(v6: i32):
    v7 = iconst.i32 1
    return v7
}
"#,
    },
    // --- Conditional unions: branch-dependent equivalences ---

    // icmp.eq v0, 42 → in true arm, v0 == 42, so iadd(v0, 8) → 50.
    // The else arm must keep iadd untouched.
    TestCase {
        name: "const_subst_eq_branch",
        clif: r#"
function %const_subst_eq_branch(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 42
    v2 = icmp.eq.i32 v0, v1
    brif v2, block1(v0), block2(v0)

block1(v3: i32):
    v4 = iconst.i32 8
    v5 = iadd.i32 v3, v4
    return v5

block2(v6: i32):
    v7 = iconst.i32 8
    v8 = iadd.i32 v6, v7
    return v8
}
"#,
    },
    // brif v0 → in true arm v0 is nonzero, so isub(v0,v0)=0 and
    // iadd(v0,0)=v0, imul(v0,v1) stays. Else arm v0==0 so imul(0,v1)=0.
    // Without path sensitivity neither arm simplifies through the phi.
    TestCase {
        name: "nonzero_from_brif",
        clif: r#"
function %nonzero_from_brif(i32, i32) -> i32 {
block0(v0: i32, v1: i32):
    brif v0, block1(v0, v1), block2(v1)

block1(v2: i32, v3: i32):
    v4 = iconst.i32 1
    v5 = imul.i32 v2, v4
    return v5

block2(v6: i32):
    return v6
}
"#,
    },
    // --- Conditional union with asymmetric diamond + phi ---

    // True arm: v0 known >= 0, sdiv(v0,4) → ushr(v0,2).
    // False arm: sdiv(v0,4) stays.
    // Both feed into a phi at block3.  The conditional union lets the
    // extractor pick ushr on the true path only.
    TestCase {
        name: "sdiv_conditional_diamond",
        clif: r#"
function %sdiv_conditional_diamond(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 0
    v2 = icmp.sge.i32 v0, v1
    brif v2, block1(v0), block2(v0)

block1(v3: i32):
    v4 = iconst.i32 4
    v5 = sdiv.i32 v3, v4
    jump block3(v5)

block2(v6: i32):
    v7 = iconst.i32 4
    v8 = sdiv.i32 v6, v7
    jump block3(v8)

block3(v9: i32):
    return v9
}
"#,
    },
    // --- Singleton range from equality comparison ---

    // icmp.eq v0, 0 → true arm: v0 == 0, so imul(v0, v1) → 0.
    TestCase {
        name: "zero_from_eq_guard",
        clif: r#"
function %zero_from_eq_guard(i32, i32) -> i32 {
block0(v0: i32, v1: i32):
    v2 = iconst.i32 0
    v3 = icmp.eq.i32 v0, v2
    brif v3, block1(v0, v1), block2(v0, v1)

block1(v4: i32, v5: i32):
    v6 = imul.i32 v4, v5
    return v6

block2(v7: i32, v8: i32):
    v9 = iadd.i32 v7, v8
    return v9
}
"#,
    },
    // --- Range propagation through block parameters ---

    // v0 < 4 && v0 >= 0 checked in block0.  Block1 receives v0 as a param.
    // Range [0,3] propagates through the param, so irem(param, 8) == param.
    // Without path sensitivity the param has no range.
    TestCase {
        name: "range_through_phi",
        clif: r#"
function %range_through_phi(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 4
    v2 = icmp.ult.i32 v0, v1
    v3 = iconst.i32 0
    v4 = icmp.sge.i32 v0, v3
    v5 = band.i32 v2, v4
    brif v5, block1(v0), block2(v0)

block1(v6: i32):
    v7 = iconst.i32 8
    v8 = irem.i32 v6, v7
    return v8

block2(v9: i32):
    return v9
}
"#,
    },
    // --- Chained conditional: two successive guards ---

    // First branch: v0 >= 0.  Second branch: v0 < 16.
    // Innermost block has range [0,15], so irem(v0, 32) == v0
    // and sdiv(v0, 4) → ushr(v0, 2).  Neither fires without scoped ranges.
    TestCase {
        name: "chained_guards",
        clif: r#"
function %chained_guards(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 0
    v2 = icmp.sge.i32 v0, v1
    brif v2, block1(v0), block3(v0)

block1(v3: i32):
    v4 = iconst.i32 16
    v5 = icmp.slt.i32 v3, v4
    brif v5, block2(v3), block3(v3)

block2(v6: i32):
    v7 = iconst.i32 4
    v8 = sdiv.i32 v6, v7
    return v8

block3(v9: i32):
    return v9
}
"#,
    },
    // --- See-through phi with conditional computation ---

    // Both arms compute iadd(v1, 1) and pass it to block3 as a param.
    // Agreement through block param → param == iadd(v1,1) unconditionally.
    // But additionally, the true arm has v0 != 0, which a context-sensitive
    // pass could exploit if block3 used v0.  Here we just test the phi fold
    // is path-aware: the extraction must follow the unconditional union.
    TestCase {
        name: "phi_agreement_computed",
        clif: r#"
function %phi_agreement_computed(i32, i32) -> i32 {
block0(v0: i32, v1: i32):
    brif v0, block1(v1), block2(v1)

block1(v2: i32):
    v3 = iconst.i32 1
    v4 = iadd.i32 v2, v3
    jump block3(v4)

block2(v5: i32):
    v6 = iconst.i32 1
    v7 = iadd.i32 v5, v6
    jump block3(v7)

block3(v8: i32):
    return v8
}
"#,
    },
    // --- Redundant guard elimination ---

    // Two identical guards: v0 >= 0 in block0, then v0 >= 0 again in block1.
    // With scoped ranges the second comparison folds to true (iconst 1).
    TestCase {
        name: "redundant_guard",
        clif: r#"
function %redundant_guard(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 0
    v2 = icmp.sge.i32 v0, v1
    brif v2, block1(v0), block2(v0)

block1(v3: i32):
    v4 = iconst.i32 0
    v5 = icmp.sge.i32 v3, v4
    brif v5, block3(v3), block4(v3)

block3(v6: i32):
    return v6

block4(v7: i32):
    v8 = iconst.i32 -1
    return v8

block2(v9: i32):
    return v9
}
"#,
    },
    // --- Multiple range-conditioned ops in one block ---

    // Guard v0 in [0,15].  Then both sdiv(v0,2) → ushr and irem(v0,32) → v0
    // should fire in the same block.
    TestCase {
        name: "multi_range_ops",
        clif: r#"
function %multi_range_ops(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 16
    v2 = icmp.ult.i32 v0, v1
    v3 = iconst.i32 0
    v4 = icmp.sge.i32 v0, v3
    v5 = band.i32 v2, v4
    brif v5, block1(v0), block2(v0)

block1(v6: i32):
    v7 = iconst.i32 2
    v8 = sdiv.i32 v6, v7
    v9 = iconst.i32 32
    v10 = irem.i32 v6, v9
    v11 = iadd.i32 v8, v10
    return v11

block2(v12: i32):
    return v12
}
"#,
    },
];

pub const TEST_CASES: &[TestCase] = PATH_SENSITIVE_TESTS;

fn count_insts(layout: &Layout, dfg: &DataFlowGraph) -> usize {
    use crate::elaborate::{CostModel, DefaultCostModel};
    let model = DefaultCostModel;
    layout
        .blocks
        .iter()
        .filter_map(|b| layout.block_data.get(b))
        .flat_map(|b| b.insts.iter())
        .map(|iid| {
            let opcode = dfg.insts[iid].opcode;
            model.cost_of_opcode(opcode).0 as usize
        })
        .sum()
}

fn run_one(clif: &str, path_sensitive: bool) -> usize {
    let (dfg, layout) = parse_clif(clif).expect("parse");
    let sig_params: Vec<Type> = layout
        .entry_block()
        .and_then(|b| layout.block_data.get(&b))
        .map(|b| b.params.iter().map(|&p| dfg.value_type(p)).collect())
        .unwrap_or_default();
    let domtree = DominatorTree::from_layout(&layout, &dfg);
    let mut pass = EgraphPass::new(dfg, layout, domtree);
    pass.set_path_sensitive(path_sensitive);
    pass.run();

    let output = pass
        .layout
        .display(&pass.dfg, "test", &sig_params, Some(Type::I32));
    println!("\nOptimized CLIF:\n{}", output);

    count_insts(&pass.layout, &pass.dfg)
}

/// Run the entire benchmark suite, comparing path-sensitive vs baseline.
#[test]
pub fn run_benchmarks() {
    println!(
        "\n{:<22} {:>8} {:>10} {:>10}",
        "test", "input", "ps", "baseline"
    );
    println!("{}", "-".repeat(54));

    let (mut tot_in, mut tot_ps, mut tot_base) = (0usize, 0usize, 0usize);

    for tc in TEST_CASES {
        let (dfg, layout) = parse_clif(tc.clif).expect("parse");
        let input = count_insts(&layout, &dfg);
        let ps = run_one(tc.clif, true);
        let base = run_one(tc.clif, false);
        println!("{:<22} {:>8} {:>10} {:>10}", tc.name, input, ps, base);
        tot_in += input;
        tot_ps += ps;
        tot_base += base;
    }

    println!("{}", "-".repeat(54));
    println!(
        "{:<22} {:>8} {:>10} {:>10}",
        "TOTAL", tot_in, tot_ps, tot_base
    );
    let pct = |x: usize| 100.0 * (tot_in - x) as f64 / tot_in as f64;
    println!(
        "\nps reduction:       {} insts ({:.1}%)",
        tot_in - tot_ps,
        pct(tot_ps)
    );
    println!(
        "baseline reduction: {} insts ({:.1}%)",
        tot_in - tot_base,
        pct(tot_base)
    );
}
