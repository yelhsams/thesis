//! Integration of pattern-based rewrites with the egraph pass
//!
//! This module shows how to use the declarative pattern IR
//! instead of hardcoded rewrite rules in the egraph pass.

use crate::pattern::*;
use crate::types::*;
use std::collections::HashSet;

/// Rewrite engine: applies pattern-based rewrites to the egraph
pub struct RewriteEngine {
    /// The library of rewrite rules to apply
    library: RewriteLibrary,

    /// Statistics
    pub matches_attempted: u64,
    pub matches_succeeded: u64,
    pub rewrites_applied: u64,
}

impl RewriteEngine {
    /// Create a new rewrite engine with the standard library
    pub fn with_standard_library() -> Self {
        Self {
            library: RewriteLibrary::standard(),
            matches_attempted: 0,
            matches_succeeded: 0,
            rewrites_applied: 0,
        }
    }

    /// Create an empty rewrite engine
    pub fn new() -> Self {
        Self {
            library: RewriteLibrary { rules: Vec::new() },
            matches_attempted: 0,
            matches_succeeded: 0,
            rewrites_applied: 0,
        }
    }

    /// Add a custom rule to the engine
    pub fn add_rule(&mut self, rule: Rewrite) {
        self.library.add_rule(rule);
    }

    /// Try to apply all rewrite rules to a value
    ///
    /// Returns a set of new equivalent values produced by rewrites
    pub fn apply_rewrites(&mut self, dfg: &mut DataFlowGraph, value: ValueId) -> HashSet<ValueId> {
        let mut results = HashSet::new();

        let inst_info = if let ValueDef::Inst(inst_id) = dfg.value_def(value) {
            Some((inst_id, dfg.insts[&inst_id].clone()))
        } else {
            None
        };

        for rule in &self.library.rules.clone() {
            self.matches_attempted += 1;

            // LHS
            let matcher = PatternMatcher::new(dfg);
            if let Some(bindings) = matcher.match_pattern(&rule.lhs, value) {
                self.matches_succeeded += 1;

                // Check conditions
                if !rule.check_conditions(&bindings) {
                    println!("      Rule '{}' matched but conditions failed", rule.name);
                    continue;
                }

                // RHS
                let mut applier = PatternApplier::new(dfg);
                if let Some(new_value) = applier.apply_pattern(&rule.rhs, &bindings) {
                    // Don't apply if it produces the same value
                    if new_value == value {
                        println!("      Rule '{}' produced same value, skipping", rule.name);
                        continue;
                    }

                    // Don't apply if we already produced this value
                    if results.contains(&new_value) {
                        println!("      Rule '{}' produced duplicate, skipping", rule.name);
                        continue;
                    }

                    self.rewrites_applied += 1;
                    results.insert(new_value);

                    // What actually changed
                    if let Some((inst_id, ref inst)) = inst_info {
                        println!(
                            "      ✓ Applied '{}': {} => {}",
                            rule.name,
                            self.format_value_description(dfg, value),
                            self.format_value_description(dfg, new_value)
                        );
                    } else {
                        println!(
                            "      ✓ Applied '{}': {} => {}",
                            rule.name, value, new_value
                        );
                    }
                }
            }
        }

        results
    }

    /// Format a value with its definition for readable output
    fn format_value_description(&self, dfg: &DataFlowGraph, value: ValueId) -> String {
        match dfg.value_def(value) {
            ValueDef::Inst(inst_id) => {
                let inst = &dfg.insts[&inst_id];
                match inst.opcode {
                    Opcode::Const => {
                        format!("{} (const {})", value, inst.immediate.unwrap_or(0))
                    }
                    Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div => {
                        let left = inst
                            .args
                            .get(0)
                            .map(|v| format!("{}", v))
                            .unwrap_or("?".to_string());
                        let right = inst
                            .args
                            .get(1)
                            .map(|v| format!("{}", v))
                            .unwrap_or("?".to_string());
                        format!("{} ({} {} {})", value, inst.opcode, left, right)
                    }
                    _ => format!("{} ({:?})", value, inst.opcode),
                }
            }
            ValueDef::BlockParam(block, idx) => {
                format!("{} (param {} of {})", value, idx, block)
            }
            ValueDef::Union(left, right) => {
                format!("{} (union {} ∪ {})", value, left, right)
            }
        }
    }

    /// Print statistics
    pub fn print_stats(&self) {
        println!("\n=== Rewrite Engine Statistics ===");
        println!("Pattern matches attempted: {}", self.matches_attempted);
        println!("Pattern matches succeeded: {}", self.matches_succeeded);
        println!("Rewrites applied: {}", self.rewrites_applied);
    }
}

/// Helper function to create common domain-specific patterns
pub mod patterns {
    use super::*;

    /// Pattern for x + 0 or 0 + x
    pub fn add_zero_either_side() -> Pattern {
        Pattern::Or(vec![
            Pattern::op(Opcode::Add, vec![Pattern::var("x"), Pattern::constant(0)]),
            Pattern::op(Opcode::Add, vec![Pattern::constant(0), Pattern::var("x")]),
        ])
    }

    /// Pattern for x * 1 or 1 * x
    pub fn mul_one_either_side() -> Pattern {
        Pattern::Or(vec![
            Pattern::op(Opcode::Mul, vec![Pattern::var("x"), Pattern::constant(1)]),
            Pattern::op(Opcode::Mul, vec![Pattern::constant(1), Pattern::var("x")]),
        ])
    }

    /// Pattern for x * 0 or 0 * x
    pub fn mul_zero_either_side() -> Pattern {
        Pattern::Or(vec![
            Pattern::op(Opcode::Mul, vec![Pattern::var("x"), Pattern::constant(0)]),
            Pattern::op(Opcode::Mul, vec![Pattern::constant(0), Pattern::var("x")]),
        ])
    }

    /// Pattern for x - x
    pub fn sub_self() -> Pattern {
        Pattern::op(Opcode::Sub, vec![Pattern::var("x"), Pattern::var("x")])
    }

    /// Pattern for (x + y) + z (associativity left)
    pub fn add_assoc_left() -> Pattern {
        Pattern::op(
            Opcode::Add,
            vec![
                Pattern::op(Opcode::Add, vec![Pattern::var("x"), Pattern::var("y")]),
                Pattern::var("z"),
            ],
        )
    }

    /// Pattern for x + (y + z) (associativity right)
    pub fn add_assoc_right() -> Pattern {
        Pattern::op(
            Opcode::Add,
            vec![
                Pattern::var("x"),
                Pattern::op(Opcode::Add, vec![Pattern::var("y"), Pattern::var("z")]),
            ],
        )
    }
}

/// Example: Build a custom rewrite library for a specific domain
pub fn build_custom_library() -> RewriteLibrary {
    let mut library = RewriteLibrary::standard();

    // Add domain-specific rewrites

    // Example: Bitwise identities
    library.add_rule(
        Rewrite::new("and-self")
            .match_pattern(Pattern::op(
                Opcode::And,
                vec![Pattern::var("x"), Pattern::var("x")],
            ))
            .produce(Pattern::var("x"))
            .build(),
    );

    library.add_rule(
        Rewrite::new("or-self")
            .match_pattern(Pattern::op(
                Opcode::Or,
                vec![Pattern::var("x"), Pattern::var("x")],
            ))
            .produce(Pattern::var("x"))
            .build(),
    );

    library.add_rule(
        Rewrite::new("xor-self")
            .match_pattern(Pattern::op(
                Opcode::Xor,
                vec![Pattern::var("x"), Pattern::var("x")],
            ))
            .produce(Pattern::constant(0))
            .build(),
    );

    // Example: More complex pattern with conditions
    library.add_rule(
        Rewrite::new("distribute-mul-add")
            .match_pattern(Pattern::op(
                Opcode::Mul,
                vec![
                    Pattern::var("x"),
                    Pattern::op(Opcode::Add, vec![Pattern::var("y"), Pattern::var("z")]),
                ],
            ))
            .produce(Pattern::op(
                Opcode::Add,
                vec![
                    Pattern::op(Opcode::Mul, vec![Pattern::var("x"), Pattern::var("y")]),
                    Pattern::op(Opcode::Mul, vec![Pattern::var("x"), Pattern::var("z")]),
                ],
            ))
            .build(),
    );

    // Example: Simplify double negation in subtraction
    library.add_rule(
        Rewrite::new("sub-neg-is-add")
            .match_pattern(Pattern::op(
                Opcode::Sub,
                vec![
                    Pattern::var("x"),
                    Pattern::op(Opcode::Sub, vec![Pattern::constant(0), Pattern::var("y")]),
                ],
            ))
            .produce(Pattern::op(
                Opcode::Add,
                vec![Pattern::var("x"), Pattern::var("y")],
            ))
            .build(),
    );

    library
}

/// Example: Pattern-based constant folding
pub fn make_constant_folding_rules() -> Vec<Rewrite> {
    let mut rules = Vec::new();

    // Add two constants
    rules.push(
        Rewrite::new("fold-add-const")
            .match_pattern(Pattern::op(
                Opcode::Add,
                vec![Pattern::any_constant("a"), Pattern::any_constant("b")],
            ))
            .produce(Pattern::any_constant("result"))
            .when(Condition::Custom {
                name: "compute-add".to_string(),
                check: |bindings| {
                    // Both must be constants (checked by pattern match)
                    bindings.get_constant(&VarId::new("a")).is_some()
                        && bindings.get_constant(&VarId::new("b")).is_some()
                },
            })
            .build(),
    );

    // Multiply two constants
    rules.push(
        Rewrite::new("fold-mul-const")
            .match_pattern(Pattern::op(
                Opcode::Mul,
                vec![Pattern::any_constant("a"), Pattern::any_constant("b")],
            ))
            .produce(Pattern::any_constant("result"))
            .when(Condition::Custom {
                name: "compute-mul".to_string(),
                check: |bindings| {
                    bindings.get_constant(&VarId::new("a")).is_some()
                        && bindings.get_constant(&VarId::new("b")).is_some()
                },
            })
            .build(),
    );

    rules
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rewrite_engine() {
        let mut dfg = DataFlowGraph::new();

        // Create: x = param
        let block = BlockId(0);
        let x = dfg.make_block_param(block, 0, Type::I32);

        // Create: zero = const 0
        let const_inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, 0);
        let const_id = dfg.make_inst(const_inst);
        let zero = dfg.make_inst_result(const_id, Type::I32);

        // Create: y = x + 0
        let add_inst = Instruction::new(Opcode::Add, vec![x, zero], Type::I32);
        let add_id = dfg.make_inst(add_inst);
        let y = dfg.make_inst_result(add_id, Type::I32);

        // Apply rewrites
        let mut engine = RewriteEngine::with_standard_library();
        let results = engine.apply_rewrites(&mut dfg, y);

        // Should produce x as a rewrite result
        assert!(results.len() > 0);
        println!("✓ Rewrite engine test passed");
    }

    #[test]
    fn test_custom_library() {
        let library = build_custom_library();

        // Should have standard rules plus custom ones
        assert!(library.rules.len() > 10);

        let has_xor_self = library.rules.iter().any(|r| r.name == "xor-self");
        assert!(has_xor_self);

        println!("✓ Custom library test passed");
    }

    #[test]
    fn test_pattern_helpers() {
        // Just check that pattern helpers compile and work
        let _p1 = patterns::add_zero_either_side();
        let _p2 = patterns::mul_one_either_side();
        let _p3 = patterns::sub_self();

        println!("✓ Pattern helpers test passed");
    }
}
