//! Context-aware optimization examples using CLIF

#[cfg(test)]
use crate::clif_parser::*;
#[cfg(test)]
use crate::egraph_pass::*;
#[cfg(test)]
use crate::support::*;
#[cfg(test)]
use crate::types::*;

#[cfg(test)]
fn canonicalize_values(s: &str) -> String {
    use std::collections::HashMap;

    // Find all v<digits> tokens using a simple scanner, in order of appearance.
    let mut map: HashMap<String, String> = HashMap::new();
    let mut order: Vec<String> = Vec::new(); // insertion order
    let mut counter = 0;

    let chars: Vec<char> = s.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        if chars[i] == 'v' {
            // Check that 'v' is not part of a longer identifier (preceded by alnum)
            let preceded_by_alnum = i > 0 && chars[i - 1].is_alphanumeric();
            if !preceded_by_alnum {
                let start = i;
                i += 1;
                while i < chars.len() && chars[i].is_ascii_digit() {
                    i += 1;
                }
                if i > start + 1 {
                    // Found v<digits>
                    let token: String = chars[start..i].iter().collect();
                    if !map.contains_key(&token) {
                        let canonical = format!("v{}", counter);
                        counter += 1;
                        map.insert(token.clone(), canonical);
                        order.push(token);
                    }
                }
                continue;
            }
        }
        i += 1;
    }

    // Replace in reverse length order (longest first) to avoid substring conflicts,
    // using a two-pass approach: first replace originals with unique placeholders,
    // then replace placeholders with canonical names.
    let mut result = s.to_string();
    let mut placeholders: Vec<(String, String)> = Vec::new();
    for (idx, original) in order.iter().enumerate() {
        let placeholder = format!("__PLACEHOLDER_{}__", idx);
        result = result.replace(original.as_str(), &placeholder);
        placeholders.push((placeholder, map[original].clone()));
    }
    for (placeholder, canonical) in &placeholders {
        result = result.replace(placeholder.as_str(), canonical.as_str());
    }
    result
}

#[cfg(test)]
fn normalize(s: &str) -> String {
    let canonicalized = canonicalize_values(s);
    canonicalized
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

#[cfg(test)]
fn helper(original_clif: &str, expected_clif: &str) {
    let (dfg, layout) = parse_clif(original_clif).unwrap();

    // Infer signature from entry block params
    let sig_params: Vec<Type> = layout
        .entry_block()
        .and_then(|b| layout.block_data.get(&b))
        .map(|b| b.params.iter().map(|&p| dfg.value_type(p)).collect())
        .unwrap_or_default();

    let domtree = DominatorTree::from_layout(&layout, &dfg);
    let mut pass = EgraphPass::new(dfg, layout, domtree);
    pass.run();

    println!("\nOptimized CLIF:");
    let output = pass
        .layout
        .display(&pass.dfg, "test", &sig_params, Some(Type::I32));
    println!("{}", output);

    assert_eq!(
        normalize(&output),
        normalize(expected_clif),
        "Does not match expected CLIF"
    );
}

#[cfg(test)]
fn helper_with_params(original_clif: &str, expected_clif: &str, sig_params: &[Type]) {
    let (dfg, layout) = parse_clif(original_clif).unwrap();
    let domtree = DominatorTree::from_layout(&layout, &dfg);
    let mut pass = EgraphPass::new(dfg, layout, domtree);
    pass.run();

    println!("\nOptimized CLIF:");
    let output = pass
        .layout
        .display(&pass.dfg, "test", sig_params, Some(Type::I32));
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
                                function %test(i32) -> i32 {
                                block0(v0: i32):
                                    v1 = iconst.i32 0
                                    v2 = icmp.eq.i32 v0, v1
                                    brif v2, block1(v0), block2(v0)

                                block1(v3: i32):
                                    v1 = iconst.i32 0
                                    return v1

                                block2(v5: i32):
                                    v6 = iconst.i32 1
                                    v7 = iadd.i32 v0, v6
                                    return v7
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
                                v1 = iconst.i32 42
                                return v1
                            }
                            "#;
        helper(original_clif, expected_clif);
    }

    #[test]
    fn test_conditional_union_mod_reduction() {
        let original_clif = r#"function %test(i32) -> i32 {
                                    block0(v0: i32):
                                        v1   = iconst.i32 4
                                        v2   = icmp.ult.i32 v0, v1         ; v0 < 4
                                        v3   = iconst.i32 0
                                        v4   = icmp.sge.i32 v0, v3        ; v0 > 0
                                        v5   = band.i32 v2, v4
                                        brif v5, block1(v0), block2(v0)

                                    block1(v3: i32):
                                        v5 = irem v0, v1         ; v0 % 4  — should equal v0
                                        return v5

                                    block2(v6: i32):
                                        return v6
                                    }"#;
        let expected_clif = r#"function %test(i32) -> i32 {
                                    block0(v0: i32):
                                        return v0
                                    }"#;
        helper(original_clif, expected_clif);
    }

    #[test]
    fn test_nested_band_condition_range_propagation() {
        // band(band(icmp.ult(v0, 8), icmp.sge(v0, 0)), icmp.slt(v0, 5))
        // On the true branch: v0 < 8 AND v0 >= 0 AND v0 < 5 → v0 ∈ [0, 4]
        // So irem(v0, 8) should simplify to v0.
        let original_clif = r#"function %test(i32) -> i32 {
                                    block0(v0: i32):
                                        v1  = iconst.i32 8
                                        v2  = icmp.ult.i32 v0, v1
                                        v3  = iconst.i32 0
                                        v4  = icmp.sge.i32 v0, v3
                                        v5  = band.i32 v2, v4
                                        v6  = iconst.i32 5
                                        v7  = icmp.slt.i32 v0, v6
                                        v8  = band.i32 v5, v7
                                        brif v8, block1(v0), block2(v0)

                                    block1(v9: i32):
                                        v10 = irem v0, v1
                                        return v10

                                    block2(v11: i32):
                                        return v11
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
                            function %test(i32) -> i32 {
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
                            function %test(i32) -> i32 {
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
                            function %test(i32, i32) -> i32 {
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
                            function %test(i32, i32) -> i32 {
                            block0(v0: i32, v1: i32):
                                return v1
                            }
                            "#;

        helper(original_clif, expected_clif);
    }

    #[test]
    fn test_loop_invariant_blockparam() {
        let original_clif = r#"
                            function %test(i32) -> i32 {
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
                            function %test(i32) -> i32 {
                            block0(v0: i32):
                                return v0
                            }
                            "#;
        helper(original_clif, expected_clif);
    }

    /// Test that block-param inline propagation creates unions so that
    /// a join-point param whose sources all resolve to the same value
    /// is optimized away.
    ///
    /// v3=v0, v4=v1 (single-source params).  In block1, brif(v0==false)
    /// means v0's range is [0,0], so v3→const 0, then iadd(0, v1)→v1,
    /// so v5=v1.  In block2, v6=v1.  At block3, v7's sources are v5=v1
    /// and v6=v1 — all agree, so v7=v1.  Final: return v1.
    #[test]
    fn test_propagate_block_params_inline() {
        let original_clif = r#"
            function %test(i32, i32) -> i32 {
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
            "#;
        let (dfg, layout) = parse_clif(original_clif).unwrap();
        let domtree = DominatorTree::from_layout(&layout, &dfg);
        let mut pass = EgraphPass::new(dfg, layout, domtree);
        pass.run();

        let output =
            pass.layout
                .display(&pass.dfg, "test", &[Type::I32, Type::I32], Some(Type::I32));
        println!("\nOptimized CLIF:\n{}", output);

        // The key optimization: the return instruction should use v1
        // (the second block0 param), not v7 or any other intermediate.
        // This verifies that block-param inline propagation unified
        // v7 with v1 through the chain: v3→0, iadd(0,v1)→v1, v5→v1,
        // v6→v1, and v7's sources all agree on v1.
        assert!(
            output.contains("return v1"),
            "Expected 'return v1' in output, got:\n{}",
            output
        );
    }
}
