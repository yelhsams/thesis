//! Context-aware optimization examples using CLIF

use crate::clif_parser::*;
use crate::egraph_pass::*;
use crate::pattern::*;
use crate::range::RangeAssumptions;
use crate::support::*;
use crate::types::*;

fn canonicalize_values(s: &str) -> String {
    let mut map = std::collections::HashMap::new();
    let mut counter = 0;
    let mut result = s.to_string();

    // Collect all v# tokens in order of appearance
    let tokens: Vec<&str> = s.split_whitespace().collect();
    for token in tokens {
        // Strip trailing punctuation like commas/parens
        let clean = token.trim_matches(|c: char| !c.is_alphanumeric() && c != 'v');
        if clean.starts_with('v') && clean[1..].chars().all(|c| c.is_ascii_digit()) {
            map.entry(clean.to_string()).or_insert_with(|| {
                let name = format!("v{}", counter);
                counter += 1;
                name
            });
        }
    }
    for (original, canonical) in &map {
        result = result.replace(original.as_str(), canonical.as_str());
    }
    result
}

fn normalize(s: &str) -> String {
    let canonicalized = canonicalize_values(s);
    canonicalized
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

fn helper(original_clif: &str, expected_clif: &str) {
    let (dfg, layout) = parse_clif(original_clif).unwrap();
    let domtree = DominatorTree::from_linear_blocks(&layout.blocks);
    let mut pass = EgraphPass::new(dfg, layout, domtree);
    pass.run();

    println!("\nOptimized CLIF:");
    let output = pass
        .layout
        .display(&pass.dfg, "test", &[Type::I32], Some(Type::I32));
    println!("{}", output);

    assert_eq!(
        normalize(&output),
        normalize(expected_clif),
        "Does not match expected CLIF"
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_constant() {
        let original_clif = r#"
                        function %test(i32) -> i32 {
                        block0(v0: i32):
                            v1 = iconst.i32 42
                            return v1
                        }"#;
        helper(original_clif, original_clif);
    }

    #[test]
    fn test_addition_identity() {
        let original_clif = r#"
                function %test(i32) -> i32 {
                block0(v0: i32):
                    v1 = iconst.i32 0
                    v2 = iadd.i32 v0, v1
                    return v2
                }
                "#;
        let expected_clif = r#"
                function %test(i32) -> i32 {
                block0(v0: i32):
                    return v0
                }
                "#;
        helper(original_clif, expected_clif);
    }

    #[test]
    fn test_multiplication_by_zero() {
        let original_clif = r#"
                            function %test(i32) -> i32 {
                            block0(v0: i32):
                                v1 = iconst.i32 0
                                v2 = imul.i32 v0, v1
                                return v2
                            }
                            "#;
        let expected_clif = r#"
                            function %test(i32) -> i32 {
                            block0(v0: i32):
                                v1 = iconst.i32 0
                                return v1
                            }
                            "#;
        helper(original_clif, expected_clif);
    }

    #[test]
    fn test_conditional_constant_propagation() {
        let original_clif = r#"
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

        let expected_clif = r#"
                                function %test_eq_zero(i32) -> i32 {
                                block0(v0: i32):
                                    v1 = iconst.i32 0
                                    v2 = icmp.eq.i32 v0, v1
                                    brif v2, block1(v0), block2(v0)

                                block1(v3: i32):
                                    jump block3(v1)

                                block2(v5: i32):
                                    v6 = iconst.i32 1
                                    v7 = iadd.i32 v5, v6
                                    jump block3(v7)

                                block3(v8: i32):
                                    return v8
                                }"#;
        helper(original_clif, expected_clif);
    }

    #[test]
    fn test_self_comparison_optimization() {
        let original_clif = r#"
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

        let expected_clif = r#"
                            function %test(i32) -> i32 {
                            block0(v0: i32):
                                jump block1(v0)

                            block1(v2: i32):
                                v3 = iconst.i32 42
                                return v3
                            }
                            "#;
        helper(original_clif, expected_clif);
    }

    #[test]
    fn test_conditional_union_mod_reduction() {
        let original_clif = r#"function %test(i32) -> i32 {
                                    block0(v0: i32):
                                        v1   = iconst.i32 4
                                        v2  = icmp.ult.i32 v0, v1          ; v0 < 4 (unsigned)
                                        brif v2, block1(v0), block2(v0)

                                    block1(v3: i32):
                                        v4 = iconst.i32 4                         ; v0 ∈ [0, 3]
                                        v5 = irem v0, v4         ; v0 % 4  — should equal v0
                                        return v5

                                    block2(v6: i32):                            ; v0 ∈ [4, +∞]
                                        return v6
                                    }"#;
        let expected_clif = r#"function %test(i32) -> i32 {
                                    block0(v0: i32):
                                        return v0
                                    }"#;
        helper(original_clif, expected_clif);
    }

    #[test]
    fn test_seeing_through_blockparam_sign_bit() {
        let original_clif = r#"
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
        let expected_clif = r#"
                            function %f(i32) -> i32 {
                            block0(v0: i32):
                                v1 = iconst.i32 1
                                return v1
                            }
                            "#;
        helper(original_clif, expected_clif);
    }

    #[test]
    fn test_seeing_through_blockparam_zero_branch() {
        let original_clif = r#"
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
        let expected_clif = r#"
                            function %f(i32, i32) -> i32 {
                            block0(v0: i32, v1: i32):
                                return v1
                            }
                            "#;

        helper(original_clif, expected_clif);
    }

    #[test]
    fn test_loop_invariant_blockparam() {
        let original_clif = r#"
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
        let expected_clif = r#"
                            function %f(i32) -> i32 {
                            block0(v0: i32):
                                return v0
                            }
                            "#;
        helper(original_clif, expected_clif);
    }
}
