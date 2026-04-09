//! Benchmarking harness for comparing path-sensitive vs baseline runs.

use crate::clif_parser::parse_clif;
use crate::egraph_pass::EgraphPass;
use crate::support::DominatorTree;
use crate::types::*;
use std::time::{Duration, Instant};

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

/// Baseline (context-free) benchmarks — plain algebraic and folding
/// simplifications that don't require path sensitivity.  Both
/// `path_sensitive = true` and `false` should handle these equally.
pub const BASELINE_TESTS: &[TestCase] = &[
    // 1. Add-zero elimination
    TestCase {
        name: "add_zero",
        clif: r#"
function %add_zero(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 0
    v2 = iadd.i32 v0, v1
    v3 = iadd.i32 v1, v2
    return v3
}
"#,
    },
    // 2. Multiply by one / zero
    TestCase {
        name: "mul_identity",
        clif: r#"
function %mul_identity(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 1
    v2 = imul.i32 v0, v1
    v3 = iconst.i32 0
    v4 = imul.i32 v2, v3
    return v4
}
"#,
    },
    // 3. Strength reduction: x * 2 → x + x
    TestCase {
        name: "strength_reduce_mul2",
        clif: r#"
function %strength_reduce_mul2(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 2
    v2 = imul.i32 v0, v1
    return v2
}
"#,
    },
    // 4. Double negation: ineg(ineg(x)) → x
    TestCase {
        name: "double_negation",
        clif: r#"
function %double_negation(i32) -> i32 {
block0(v0: i32):
    v1 = ineg.i32 v0
    v2 = ineg.i32 v1
    return v2
}
"#,
    },
    // 5. AND with all-ones identity: x & -1 → x
    TestCase {
        name: "and_allones",
        clif: r#"
function %and_allones(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 -1
    v2 = band.i32 v0, v1
    return v2
}
"#,
    },
    // 6. XOR self-cancellation: x ^ x → 0
    TestCase {
        name: "xor_self",
        clif: r#"
function %xor_self(i32) -> i32 {
block0(v0: i32):
    v1 = bxor.i32 v0, v0
    return v1
}
"#,
    },
    // 7. GVN / CSE: same computation reused
    TestCase {
        name: "gvn_cse",
        clif: r#"
function %gvn_cse(i32, i32) -> i32 {
block0(v0: i32, v1: i32):
    v2 = iadd.i32 v0, v1
    v3 = iadd.i32 v0, v1
    v4 = iadd.i32 v2, v3
    return v4
}
"#,
    },
    // 8. Constant folding chain
    TestCase {
        name: "const_fold_chain",
        clif: r#"
function %const_fold_chain() -> i32 {
block0:
    v0 = iconst.i32 10
    v1 = iconst.i32 20
    v2 = iadd.i32 v0, v1
    v3 = iconst.i32 5
    v4 = imul.i32 v2, v3
    return v4
}
"#,
    },
    // 9. OR with zero identity + double bnot
    TestCase {
        name: "or_zero_double_bnot",
        clif: r#"
function %or_zero_double_bnot(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 0
    v2 = bor.i32 v0, v1
    v3 = bnot.i32 v2
    v4 = bnot.i32 v3
    return v4
}
"#,
    },
    // 10. Sub-self → 0 + add-zero cleanup
    TestCase {
        name: "sub_self_cleanup",
        clif: r#"
function %sub_self_cleanup(i32, i32) -> i32 {
block0(v0: i32, v1: i32):
    v2 = isub.i32 v0, v0
    v3 = iadd.i32 v1, v2
    return v3
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

/// Run the full compilation pipeline (parse + domtree + egraph pass) once
/// and return `(instruction_cost, wall_clock_time)`.  Used by the timing
/// benchmark to compare `path_sensitive = true` vs `false`.
fn time_one(clif: &str, path_sensitive: bool) -> (usize, Duration) {
    let (dfg, layout) = parse_clif(clif).expect("parse");
    let start = Instant::now();
    let domtree = DominatorTree::from_layout(&layout, &dfg);
    let mut pass = EgraphPass::new(dfg, layout, domtree);
    pass.set_path_sensitive(path_sensitive);
    pass.run();
    let elapsed = start.elapsed();
    let cost = count_insts(&pass.layout, &pass.dfg);
    (cost, elapsed)
}

/// Timing benchmark: runs each suite (baseline + path-sensitive) through the
/// compilation pipeline with `path_sensitive = true` and `false`, taking the
/// best of several repetitions to reduce noise, and prints wall-clock
/// comparisons.
#[test]
pub fn time_benchmarks() {
    // Repetitions per configuration — take the minimum to get a stable
    // lower-bound on wall-clock time while ignoring transient spikes.
    const REPS: usize = 25;

    fn bench_suite(name: &str, suite: &[TestCase]) -> (Duration, Duration, usize, usize) {
        println!(
            "\n=== {} suite ({} tests) ===\n{:<24} {:>12} {:>12} {:>10}",
            name,
            suite.len(),
            "test",
            "ps (µs)",
            "base (µs)",
            "speedup"
        );
        println!("{}", "-".repeat(62));

        let mut tot_ps = Duration::ZERO;
        let mut tot_base = Duration::ZERO;
        let mut cost_ps = 0usize;
        let mut cost_base = 0usize;

        for tc in suite {
            // Warm-up to prime any lazy caches / allocators.
            let _ = time_one(tc.clif, true);
            let _ = time_one(tc.clif, false);

            let mut best_ps = Duration::MAX;
            let mut best_base = Duration::MAX;
            let mut last_cost_ps = 0usize;
            let mut last_cost_base = 0usize;
            for _ in 0..REPS {
                let (c, t) = time_one(tc.clif, true);
                if t < best_ps {
                    best_ps = t;
                }
                last_cost_ps = c;
                let (c, t) = time_one(tc.clif, false);
                if t < best_base {
                    best_base = t;
                }
                last_cost_base = c;
            }

            let ps_us = best_ps.as_secs_f64() * 1e6;
            let base_us = best_base.as_secs_f64() * 1e6;
            let speedup = if ps_us > 0.0 { base_us / ps_us } else { 0.0 };
            println!(
                "{:<24} {:>12.2} {:>12.2} {:>9.2}x",
                tc.name, ps_us, base_us, speedup
            );

            tot_ps += best_ps;
            tot_base += best_base;
            cost_ps += last_cost_ps;
            cost_base += last_cost_base;
        }

        println!("{}", "-".repeat(62));
        let tot_ps_us = tot_ps.as_secs_f64() * 1e6;
        let tot_base_us = tot_base.as_secs_f64() * 1e6;
        let tot_speedup = if tot_ps_us > 0.0 {
            tot_base_us / tot_ps_us
        } else {
            0.0
        };
        println!(
            "{:<24} {:>12.2} {:>12.2} {:>9.2}x",
            "TOTAL", tot_ps_us, tot_base_us, tot_speedup
        );
        println!(
            "  final cost: ps={} baseline={} (lower = better)",
            cost_ps, cost_base
        );

        (tot_ps, tot_base, cost_ps, cost_base)
    }

    let (base_ps, base_base, bc_ps, bc_base) = bench_suite("BASELINE", BASELINE_TESTS);
    let (ps_ps, ps_base, pc_ps, pc_base) = bench_suite("PATH-SENSITIVE", PATH_SENSITIVE_TESTS);

    println!("\n=== SUMMARY ===");
    println!(
        "{:<24} {:>12} {:>12} {:>10}",
        "suite", "ps (µs)", "base (µs)", "overhead"
    );
    println!("{}", "-".repeat(62));
    let fmt = |name: &str, ps: Duration, base: Duration| {
        let ps_us = ps.as_secs_f64() * 1e6;
        let base_us = base.as_secs_f64() * 1e6;
        let overhead = if base_us > 0.0 {
            100.0 * (ps_us - base_us) / base_us
        } else {
            0.0
        };
        println!(
            "{:<24} {:>12.2} {:>12.2} {:>+9.1}%",
            name, ps_us, base_us, overhead
        );
    };
    fmt("BASELINE", base_ps, base_base);
    fmt("PATH-SENSITIVE", ps_ps, ps_base);
    fmt("COMBINED", base_ps + ps_ps, base_base + ps_base);

    println!("\n=== COST REDUCTION ===");
    println!(
        "BASELINE        : ps={} base={}  (Δ={})",
        bc_ps,
        bc_base,
        bc_base as i64 - bc_ps as i64
    );
    println!(
        "PATH-SENSITIVE  : ps={} base={}  (Δ={})",
        pc_ps,
        pc_base,
        pc_base as i64 - pc_ps as i64
    );
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
