//! Benchmarking harness for comparing path-sensitive vs baseline runs.

use crate::clif_parser::parse_clif;
use crate::egraph_pass::EgraphPass;
use crate::support::DominatorTree;
use crate::types::Layout;

pub struct TestCase {
    pub name: &'static str,
    pub clif: &'static str,
}

pub const TEST_CASES: &[TestCase] = &[
    TestCase {
        name: "algebra",
        clif: r#"
function %bench_algebra(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 0
    v2 = iadd.i32 v0, v1
    v3 = iconst.i32 1
    v4 = imul.i32 v2, v3
    v5 = isub.i32 v4, v1
    return v5
}
"#,
    },
    TestCase {
        name: "gvn",
        clif: r#"
function %bench_gvn(i32, i32) -> i32 {
block0(v0: i32, v1: i32):
    v2 = iadd.i32 v0, v1
    v3 = imul.i32 v2, v2
    jump block1

block1():
    v4 = iadd.i32 v0, v1
    v5 = imul.i32 v4, v4
    v6 = iadd.i32 v3, v5
    return v6
}
"#,
    },
    TestCase {
        name: "ccp_zero",
        clif: r#"
function %bench_ccp_zero(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 0
    v2 = icmp.eq.i32 v0, v1
    brif v2, block1(v0), block2(v0)

block1(v3: i32):
    v4 = iadd.i32 v3, v3
    return v4

block2(v5: i32):
    v6 = iconst.i32 1
    v7 = iadd.i32 v5, v6
    return v7
}
"#,
    },
    TestCase {
        name: "irem_range",
        clif: r#"
function %bench_irem_range(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 4
    v2 = icmp.ult.i32 v0, v1
    v3 = iconst.i32 0
    v4 = icmp.sge.i32 v0, v3
    v5 = band.i32 v2, v4
    brif v5, block1(v0), block2(v0)

block1(v6: i32):
    v7 = iconst.i32 8
    v8 = irem.i32 v0, v7
    return v8

block2(v9: i32):
    return v9
}
"#,
    },
    TestCase {
        name: "nested_range",
        clif: r#"
function %bench_nested_range(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 8
    v2 = icmp.ult.i32 v0, v1
    v3 = iconst.i32 0
    v4 = icmp.sge.i32 v0, v3
    v5 = band.i32 v2, v4
    v6 = iconst.i32 5
    v7 = icmp.slt.i32 v0, v6
    v8 = band.i32 v5, v7
    brif v8, block1(v0), block2(v0)

block1(v9: i32):
    v10 = irem.i32 v0, v1
    return v10

block2(v11: i32):
    return v11
}
"#,
    },
    TestCase {
        name: "phi_agreement",
        clif: r#"
function %bench_phi_agreement(i32, i32) -> i32 {
block0(v0: i32, v1: i32):
    brif v0, block1(v1), block2(v1)

block1(v2: i32):
    jump block3(v2)

block2(v3: i32):
    jump block3(v3)

block3(v4: i32):
    return v4
}
"#,
    },
    TestCase {
        name: "phi_fold",
        clif: r#"
function %bench_phi_fold(i32, i32) -> i32 {
block0(v0: i32, v1: i32):
    brif v0, block2(v1), block1(v0, v1)

block1(v3: i32, v4: i32):
    v5 = iadd.i32 v3, v4
    jump block3(v5)

block2(v6: i32):
    jump block3(v6)

block3(v7: i32):
    return v7
}
"#,
    },
    TestCase {
        name: "self_cmp",
        clif: r#"
function %bench_self_cmp(i32) -> i32 {
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
"#,
    },
    TestCase {
        name: "deep_diamond",
        clif: r#"
function %bench_deep_diamond(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 0
    v2 = icmp.ne.i32 v0, v1
    brif v2, block1(v0), block3(v0)

block1(v3: i32):
    v4 = iconst.i32 0
    v5 = icmp.sge.i32 v3, v4
    brif v5, block2(v3), block3(v3)

block2(v6: i32):
    v7 = iconst.i32 1
    v8 = imul.i32 v6, v7
    v9 = iadd.i32 v8, v1
    return v9

block3(v10: i32):
    return v10
}
"#,
    },
    TestCase {
        name: "three_arm_phi",
        clif: r#"
function %bench_three_arm_phi(i32, i32) -> i32 {
block0(v0: i32, v1: i32):
    v2 = iconst.i32 1
    v3 = icmp.eq.i32 v0, v2
    brif v3, block1(v1), block5(v0, v1)

block5(v4: i32, v5: i32):
    v6 = iconst.i32 2
    v7 = icmp.eq.i32 v4, v6
    brif v7, block2(v5), block3(v5)

block1(v8: i32):
    jump block4(v8)

block2(v9: i32):
    jump block4(v9)

block3(v10: i32):
    jump block4(v10)

block4(v11: i32):
    return v11
}
"#,
    },
    TestCase {
        name: "constfold",
        clif: r#"
function %bench_constfold(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 3
    v2 = iconst.i32 7
    v3 = iadd.i32 v1, v2
    v4 = iconst.i32 2
    v5 = imul.i32 v3, v4
    v6 = iadd.i32 v0, v5
    return v6
}
"#,
    },
    TestCase {
        name: "double_neg",
        clif: r#"
function %bench_double_neg(i32) -> i32 {
block0(v0: i32):
    v1 = ineg.i32 v0
    v2 = ineg.i32 v1
    return v2
}
"#,
    },
    TestCase {
        name: "and_ones",
        clif: r#"
function %bench_and_ones(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 -1
    v2 = band.i32 v0, v1
    v3 = bor.i32 v2, v0
    return v3
}
"#,
    },
    TestCase {
        name: "deep_gvn",
        clif: r#"
function %bench_deep_gvn(i32, i32) -> i32 {
block0(v0: i32, v1: i32):
    v2 = iadd.i32 v0, v1
    v3 = imul.i32 v2, v0
    v4 = iadd.i32 v0, v1
    v5 = imul.i32 v4, v0
    v6 = iadd.i32 v0, v1
    v7 = iadd.i32 v3, v5
    v8 = iadd.i32 v7, v6
    return v8
}
"#,
    },
    TestCase {
        name: "sdiv_to_ushr",
        clif: r#"
function %bench_sdiv_to_ushr(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 0
    v2 = icmp.sge.i32 v0, v1
    brif v2, block1(v0), block2(v0)

block1(v3: i32):
    v4 = iconst.i32 4
    v5 = sdiv.i32 v3, v4
    return v5

block2(v6: i32):
    v7 = iconst.i32 4
    v8 = sdiv.i32 v6, v7
    return v8
}
"#,
    },
    TestCase {
        name: "const_prop_diamond",
        clif: r#"
function %bench_const_prop_diamond(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 42
    v2 = icmp.eq.i32 v0, v1
    brif v2, block1(v0), block2(v0)

block1(v3: i32):
    v4 = iconst.i32 8
    v5 = iadd.i32 v3, v4
    return v5

block2(v6: i32):
    return v6
}
"#,
    },
    TestCase {
        name: "nonzero_chain",
        clif: r#"
function %bench_nonzero_chain(i32, i32) -> i32 {
block0(v0: i32, v1: i32):
    brif v0, block1(v0, v1), block2(v0)

block1(v2: i32, v3: i32):
    v4 = isub.i32 v2, v2
    v5 = iadd.i32 v2, v4
    v6 = imul.i32 v5, v3
    return v6

block2(v7: i32):
    return v7
}
"#,
    },
    TestCase {
        name: "asymmetric_diamond",
        clif: r#"
function %bench_asymmetric_diamond(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 10
    v2 = icmp.ult.i32 v0, v1
    v3 = iconst.i32 0
    v4 = icmp.sge.i32 v0, v3
    v5 = band.i32 v2, v4
    brif v5, block1(v0), block2(v0)

block1(v6: i32):
    v7 = iconst.i32 16
    v8 = irem.i32 v6, v7
    jump block3(v8)

block2(v9: i32):
    jump block3(v9)

block3(v10: i32):
    return v10
}
"#,
    },
];

fn count_insts(layout: &Layout) -> usize {
    layout.block_data.values().map(|b| b.insts.len()).sum()
}

fn run_one(clif: &str, path_sensitive: bool) -> usize {
    let (dfg, layout) = parse_clif(clif).expect("parse");
    let domtree = DominatorTree::from_layout(&layout, &dfg);
    let mut pass = EgraphPass::new(dfg, layout, domtree);
    pass.set_path_sensitive(path_sensitive);
    pass.run();
    count_insts(&pass.layout)
}

/// Run the entire benchmark suite, comparing path-sensitive vs baseline.
pub fn run_benchmarks() {
    println!(
        "\n{:<22} {:>8} {:>10} {:>10}",
        "test", "input", "ps", "baseline"
    );
    println!("{}", "-".repeat(54));

    let (mut tot_in, mut tot_ps, mut tot_base) = (0usize, 0usize, 0usize);

    for tc in TEST_CASES {
        let (_, layout) = parse_clif(tc.clif).expect("parse");
        let input = count_insts(&layout);
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
