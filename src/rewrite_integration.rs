//! Integration of pattern-based rewrites with the egraph pass
//!
//! This module shows how to use the declarative pattern IR
//! instead of hardcoded rewrite rules in the egraph pass.

use crate::pattern::*;
use crate::types::*;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::HashSet;

/// Rewrite engine: applies pattern-based rewrites to the egraph
pub struct RewriteEngine {
    /// The library of rewrite rules to apply
    pub library: RewriteLibrary,

    /// Indices into `library.rules` of rules that contain at least one
    /// range-based condition, paired with the LHS top-level opcode (if
    /// the LHS is a concrete `Pattern::Op { .. }`).  Precomputed so
    /// range-conditioned rewrites don't need to scan the full rule
    /// library per pure instruction, and can skip rules whose LHS cannot
    /// possibly match the current instruction's opcode.
    pub range_rule_indices: Vec<(usize, Option<Opcode>)>,

    /// Set of LHS top-level opcodes that appear in at least one
    /// range-conditioned rule.  When a value's defining opcode is not
    /// in this set (and no unconstrained range rules exist), we can
    /// skip the range-conditioned rewrite pass entirely.
    pub range_rule_opcodes: FxHashSet<Opcode>,
    pub has_unconstrained_range_rule: bool,

    /// Index of rules bucketed by the LHS's top-level opcode.  Rules
    /// whose LHS is a concrete `Pattern::Op { opcode, .. }` are placed
    /// in `rules_by_opcode[opcode]`; rules whose LHS has no concrete
    /// top-level opcode (Or / Var / Where / AnyConstant / ...) go into
    /// `unconstrained_rule_indices`, which are tried for every value.
    pub rules_by_opcode: FxHashMap<Opcode, Vec<usize>>,
    pub unconstrained_rule_indices: Vec<usize>,

    /// Statistics
    pub matches_attempted: u64,
    pub matches_succeeded: u64,
    pub rewrites_applied: u64,
}

impl RewriteEngine {
    /// Create a new rewrite engine with the standard library
    pub fn with_standard_library() -> Self {
        let library = RewriteLibrary::standard();
        let range_rule_indices = Self::compute_range_rule_indices(&library.rules);
        let (rules_by_opcode, unconstrained_rule_indices) =
            Self::bucket_rules_by_opcode(&library.rules);
        let (range_rule_opcodes, has_unconstrained_range_rule) =
            Self::compute_range_rule_opcode_set(&range_rule_indices);
        Self {
            library,
            range_rule_indices,
            rules_by_opcode,
            unconstrained_rule_indices,
            range_rule_opcodes,
            has_unconstrained_range_rule,
            matches_attempted: 0,
            matches_succeeded: 0,
            rewrites_applied: 0,
        }
    }

    /// Create an empty rewrite engine
    pub fn new() -> Self {
        Self {
            library: RewriteLibrary { rules: Vec::new() },
            range_rule_indices: Vec::new(),
            rules_by_opcode: FxHashMap::default(),
            unconstrained_rule_indices: Vec::new(),
            range_rule_opcodes: FxHashSet::default(),
            has_unconstrained_range_rule: false,
            matches_attempted: 0,
            matches_succeeded: 0,
            rewrites_applied: 0,
        }
    }

    /// Add a custom rule to the engine
    pub fn add_rule(&mut self, rule: Rewrite) {
        let idx = self.library.rules.len();
        let has_range = Self::rule_has_range_condition(&rule);
        let top_op = Self::lhs_top_opcode(&rule.lhs);
        self.library.add_rule(rule);
        if has_range {
            self.range_rule_indices.push((idx, top_op));
            match top_op {
                Some(op) => {
                    self.range_rule_opcodes.insert(op);
                }
                None => {
                    self.has_unconstrained_range_rule = true;
                }
            }
        }
        match top_op {
            Some(op) => {
                self.rules_by_opcode.entry(op).or_default().push(idx);
            }
            None => {
                self.unconstrained_rule_indices.push(idx);
            }
        }
    }

    fn compute_range_rule_opcode_set(
        range_rule_indices: &[(usize, Option<Opcode>)],
    ) -> (FxHashSet<Opcode>, bool) {
        let mut set = FxHashSet::default();
        let mut has_unconstrained = false;
        for &(_, top) in range_rule_indices {
            match top {
                Some(op) => {
                    set.insert(op);
                }
                None => has_unconstrained = true,
            }
        }
        (set, has_unconstrained)
    }

    fn bucket_rules_by_opcode(rules: &[Rewrite]) -> (FxHashMap<Opcode, Vec<usize>>, Vec<usize>) {
        let mut by_op: FxHashMap<Opcode, Vec<usize>> = FxHashMap::default();
        let mut unconstrained: Vec<usize> = Vec::new();
        for (i, r) in rules.iter().enumerate() {
            match Self::lhs_top_opcode(&r.lhs) {
                Some(op) => by_op.entry(op).or_default().push(i),
                None => unconstrained.push(i),
            }
        }
        (by_op, unconstrained)
    }

    fn compute_range_rule_indices(rules: &[Rewrite]) -> Vec<(usize, Option<Opcode>)> {
        rules
            .iter()
            .enumerate()
            .filter(|(_, r)| Self::rule_has_range_condition(r))
            .map(|(i, r)| (i, Self::lhs_top_opcode(&r.lhs)))
            .collect()
    }

    /// Return the top-level opcode of an LHS pattern if it's a concrete
    /// `Pattern::Op`, else `None` (means "may match any opcode").
    fn lhs_top_opcode(pattern: &Pattern) -> Option<Opcode> {
        match pattern {
            Pattern::Op { opcode, .. } => Some(*opcode),
            _ => None,
        }
    }

    fn rule_has_range_condition(rule: &Rewrite) -> bool {
        fn walk(conds: &[Condition]) -> bool {
            for c in conds {
                match c {
                    Condition::InRange(..)
                    | Condition::NonNegative(..)
                    | Condition::Positive(..)
                    | Condition::Negative(..)
                    | Condition::RangeLessThan(..)
                    | Condition::RangeGreaterThan(..)
                    | Condition::RangeEquals(..)
                    | Condition::Unreachable(..)
                    | Condition::NonZero(..)
                    | Condition::Custom { .. } => return true,
                    Condition::And(inner) => {
                        if walk(inner) {
                            return true;
                        }
                    }
                    _ => {}
                }
            }
            false
        }
        walk(&rule.conditions)
    }

    /// Try to apply all rewrite rules to a value (without range info)
    ///
    /// Returns a set of new equivalent values produced by rewrites
    pub fn apply_rewrites(&mut self, dfg: &mut DataFlowGraph, value: ValueId) -> HashSet<ValueId> {
        self.apply_rewrites_with_ranges(dfg, value, None)
    }

    /// Try to apply all rewrite rules to a value, with optional range assumptions
    ///
    /// When range_assumptions is provided, range-based conditions (NonNegative,
    /// Positive, InRange, etc.) can fire during rewrite rule matching.
    pub fn apply_rewrites_with_ranges(
        &mut self,
        dfg: &mut DataFlowGraph,
        value: ValueId,
        range_assumptions: Option<&crate::range::RangeAssumptions>,
    ) -> HashSet<ValueId> {
        let mut results = HashSet::new();

        // Resolve the defining instruction's opcode so we can look up
        // only rules that could possibly match this value.
        let value_opcode = match dfg.value_def(value) {
            ValueDef::Inst(iid) => Some(dfg.insts[&iid].opcode),
            _ => None,
        };

        // Temporarily move `rules` out so we can iterate over it while still
        // mutating other fields of `self` (matches_attempted, etc.).  This
        // avoids cloning the full rule library per call — a major hot spot
        // for path-sensitive compilation.
        let rules = std::mem::take(&mut self.library.rules);

        // Helper closure to try a single rule; pulled out so it can be
        // called for both the opcode-specific bucket and the
        // "unconstrained" bucket (Or / Var / Where / AnyConstant LHSs).
        let mut try_rule = |engine: &mut Self, rule: &Rewrite, dfg: &mut DataFlowGraph| {
            engine.matches_attempted += 1;
            let matcher = PatternMatcher::new(dfg);
            if let Some(mut bindings) = matcher.match_pattern(&rule.lhs, value) {
                engine.matches_succeeded += 1;

                if let Some(ra) = range_assumptions {
                    bindings.set_range_assumptions(ra);
                }

                if !rule.check_conditions(&bindings) {
                    return;
                }

                let mut applier = PatternApplier::new(dfg);
                if let Some(new_value) = applier.apply_pattern(&rule.rhs, &bindings) {
                    if new_value == value {
                        return;
                    }
                    if results.contains(&new_value) {
                        return;
                    }
                    engine.rewrites_applied += 1;
                    results.insert(new_value);
                }
            }
        };

        // Try opcode-specific rules for this value's opcode.
        if let Some(op) = value_opcode {
            // Clone the index list to avoid holding a borrow on
            // `self.rules_by_opcode` while calling `try_rule`, which
            // mutates other fields of `self`.  The indices list is small.
            if let Some(idx_list) = self.rules_by_opcode.get(&op).cloned() {
                for idx in idx_list {
                    let rule = &rules[idx];
                    try_rule(self, rule, dfg);
                }
            }
        }

        // Then try unconstrained rules (Or / Var / Where / AnyConstant).
        let unconstrained = self.unconstrained_rule_indices.clone();
        for idx in unconstrained {
            let rule = &rules[idx];
            try_rule(self, rule, dfg);
        }

        self.library.rules = rules;
        results
    }

    /// Print statistics
    pub fn print_stats(&self) {
        eprintln!("\n=== Rewrite Engine Statistics ===");
        eprintln!("Pattern matches attempted: {}", self.matches_attempted);
        eprintln!("Pattern matches succeeded: {}", self.matches_succeeded);
        eprintln!("Rewrites applied: {}", self.rewrites_applied);
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
