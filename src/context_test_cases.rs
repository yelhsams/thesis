//! Context-aware optimization examples using CLIF

#[cfg(test)]
mod tests {
    use crate::clif_parser::*;
    use crate::egraph_pass::*;
    use crate::support::*;
    use crate::types::*;
    use std::collections::HashMap;

    /// Rename `v<digits>` tokens in order of first appearance.
    fn canonicalize_values(s: &str) -> String {
        let mut map: HashMap<String, String> = HashMap::new();
        let mut out = String::with_capacity(s.len());
        let bytes = s.as_bytes();
        let mut i = 0;
        let mut prev_alnum = false;
        while i < bytes.len() {
            let c = bytes[i];
            if c == b'v' && !prev_alnum && i + 1 < bytes.len() && bytes[i + 1].is_ascii_digit() {
                let start = i;
                i += 1;
                while i < bytes.len() && bytes[i].is_ascii_digit() {
                    i += 1;
                }
                let tok = &s[start..i];
                let next = map.len();
                let canonical = map
                    .entry(tok.to_string())
                    .or_insert_with(|| format!("v{}", next));
                out.push_str(canonical);
                prev_alnum = true;
            } else {
                out.push(c as char);
                prev_alnum = (c as char).is_alphanumeric();
                i += 1;
            }
        }
        out
    }

    fn normalize(s: &str) -> String {
        canonicalize_values(s)
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ")
    }

    fn baseline_mode() -> bool {
        std::env::var("THESIS_PATH_SENSITIVE")
            .map(|v| v == "0" || v.eq_ignore_ascii_case("false"))
            .unwrap_or(false)
    }

    fn run_pass(clif: &str) -> (EgraphPass, Vec<Type>) {
        let (dfg, layout) = parse_clif(clif).unwrap();
        let sig_params: Vec<Type> = layout
            .entry_block()
            .and_then(|b| layout.block_data.get(&b))
            .map(|b| b.params.iter().map(|&p| dfg.value_type(p)).collect())
            .unwrap_or_default();
        let domtree = DominatorTree::from_layout(&layout, &dfg);
        let mut pass = EgraphPass::new(dfg, layout, domtree);
        pass.set_path_sensitive(!baseline_mode());
        pass.run();
        (pass, sig_params)
    }

    fn assert_baseline_invariants(pass: &EgraphPass) {
        assert!(pass.dfg.conditional_unions.is_empty());
        assert!(pass.block_entry_facts.is_empty());
    }

    fn helper(original_clif: &str, expected_clif: &str) {
        let (pass, sig_params) = run_pass(original_clif);
        if baseline_mode() {
            assert_baseline_invariants(&pass);
            return;
        }
        let output = pass
            .layout
            .display(&pass.dfg, "test", &sig_params, Some(Type::I32));
        println!("\nOptimized CLIF:\n{}", output);
        assert_eq!(normalize(&output), normalize(expected_clif), "mismatch");
    }

    #[test]
    fn test_simple_constant() {
        let clif = r#"
            function %test(i32) -> i32 {
            block0(v0: i32):
                v1 = iconst.i32 42
                return v1
            }"#;
        helper(clif, clif);
    }

    #[test]
    fn test_addition_identity() {
        helper(
            r#"
            function %test(i32) -> i32 {
            block0(v0: i32):
                v1 = iconst.i32 0
                v2 = iadd.i32 v0, v1
                return v2
            }"#,
            r#"
            function %test(i32) -> i32 {
            block0(v0: i32):
                return v0
            }"#,
        );
    }

    #[test]
    fn test_multiplication_by_zero() {
        helper(
            r#"
            function %test(i32) -> i32 {
            block0(v0: i32):
                v1 = iconst.i32 0
                v2 = imul.i32 v0, v1
                return v2
            }"#,
            r#"
            function %test(i32) -> i32 {
            block0(v0: i32):
                v1 = iconst.i32 0
                return v1
            }"#,
        );
    }

    #[test]
    fn test_conditional_constant_propagation() {
        helper(
            r#"
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
            }"#,
            r#"
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
            }"#,
        );
    }

    #[test]
    fn test_self_comparison_optimization() {
        helper(
            r#"
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
            }"#,
            r#"
            function %test(i32) -> i32 {
            block0(v0: i32):
                v1 = iconst.i32 42
                return v1
            }"#,
        );
    }

    #[test]
    fn test_conditional_union_mod_reduction() {
        helper(
            r#"function %test(i32) -> i32 {
                block0(v0: i32):
                    v1 = iconst.i32 4
                    v2 = icmp.ult.i32 v0, v1
                    v3 = iconst.i32 0
                    v4 = icmp.sge.i32 v0, v3
                    v5 = band.i32 v2, v4
                    brif v5, block1(v0), block2(v0)

                block1(v3: i32):
                    v5 = irem v0, v1
                    return v5

                block2(v6: i32):
                    return v6
                }"#,
            r#"function %test(i32) -> i32 {
                block0(v0: i32):
                    return v0
                }"#,
        );
    }

    #[test]
    fn test_nested_band_condition_range_propagation() {
        helper(
            r#"function %test(i32) -> i32 {
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
                    v10 = irem v0, v1
                    return v10

                block2(v11: i32):
                    return v11
                }"#,
            r#"function %test(i32) -> i32 {
                block0(v0: i32):
                    return v0
                }"#,
        );
    }

    #[test]
    fn test_seeing_through_blockparam_sign_bit() {
        helper(
            r#"
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
            }"#,
            r#"
            function %test(i32) -> i32 {
            block0(v0: i32):
                v1 = iconst.i32 1
                return v1
            }"#,
        );
    }

    #[test]
    fn test_seeing_through_blockparam_zero_branch() {
        helper(
            r#"
            function %test(i32, i32) -> i32 {
            block0(v0: i32, v1: i32):
                brif v0, block2(v1), block1(v1)

            block1(v2: i32):
                jump block3(v2)

            block2(v3: i32):
                jump block3(v3)

            block3(v4: i32):
                return v4
            }"#,
            r#"
            function %test(i32, i32) -> i32 {
            block0(v0: i32, v1: i32):
                return v1
            }"#,
        );
    }

    #[test]
    fn test_loop_invariant_blockparam() {
        helper(
            r#"
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
            }"#,
            r#"
            function %test(i32) -> i32 {
            block0(v0: i32):
                return v0
            }"#,
        );
    }

    #[test]
    fn test_propagate_block_params_inline() {
        let clif = r#"
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
            }"#;
        let (pass, sig_params) = run_pass(clif);
        if baseline_mode() {
            assert_baseline_invariants(&pass);
            return;
        }
        let output = pass
            .layout
            .display(&pass.dfg, "test", &sig_params, Some(Type::I32));
        println!("\nOptimized CLIF:\n{}", output);
        assert!(
            output.contains("return v1"),
            "expected 'return v1' in:\n{}",
            output
        );
    }
}
