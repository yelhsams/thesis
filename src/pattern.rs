//! Pattern IR for rewrite rules.
//!
//! Example: `(x + 0) => x`
//! ```ignore
//! let rule = Rewrite::new("add-zero")
//!     .match_pattern(
//!         Pattern::op(Opcode::Add, vec![
//!             Pattern::var("x"),
//!             Pattern::constant(0),
//!         ])
//!     )
//!     .produce(Pattern::var("x"))
//!     .build();
//! ```

use crate::range::{Range, RangeAssumptions};
use crate::types::*;
use std::collections::HashMap;

/// Pattern variable identifier (e.g. "x", "y").
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VarId(pub String);

impl VarId {
    pub fn new(s: impl Into<String>) -> Self {
        VarId(s.into())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// Matches anything, binds to a variable.
    Var(VarId),

    /// Matches an instruction with this opcode and matching args.
    Op { opcode: Opcode, args: Vec<Pattern> },

    /// Matches a specific constant.
    Constant(i64),

    /// Matches any constant, binds its value.
    AnyConstant(VarId),

    /// Matches one of several alternatives.
    Or(Vec<Pattern>),
}

impl Pattern {
    pub fn var(name: impl Into<String>) -> Self {
        Pattern::Var(VarId::new(name))
    }

    pub fn op(opcode: Opcode, args: Vec<Pattern>) -> Self {
        Pattern::Op { opcode, args }
    }

    pub fn constant(value: i64) -> Self {
        Pattern::Constant(value)
    }

    pub fn any_constant(name: impl Into<String>) -> Self {
        Pattern::AnyConstant(VarId::new(name))
    }
}

/// Side conditions on a rewrite.
#[derive(Debug, Clone, PartialEq)]
pub enum Condition {
    /// All inner conditions must hold.
    And(Vec<Condition>),

    /// User-defined check.
    Custom {
        name: String,
        check: fn(&Bindings) -> bool,
    },

    InRange(VarId, i64, i64),
    NonNegative(VarId),
    Positive(VarId),
    Negative(VarId),
    RangeLessThan(VarId, i64),
    RangeGreaterThan(VarId, i64),
    RangeEquals(VarId, i64),
    Unreachable(VarId),

    NonZero(VarId),
}

/// Pattern variable -> value/constant bindings.
#[derive(Debug, Clone)]
pub struct Bindings {
    pub values: HashMap<VarId, ValueId>,
    pub constants: HashMap<VarId, i64>,

    dfg: *const DataFlowGraph,
    range_assumptions: Option<*const RangeAssumptions>,
}

impl Bindings {
    pub fn new(dfg: &DataFlowGraph) -> Self {
        Self {
            values: HashMap::new(),
            constants: HashMap::new(),
            dfg: dfg as *const _,
            range_assumptions: None,
        }
    }

    pub fn set_range_assumptions(&mut self, range_assumptions: &RangeAssumptions) {
        self.range_assumptions = Some(range_assumptions as *const _);
    }

    pub fn range_assumptions(&self) -> Option<&RangeAssumptions> {
        self.range_assumptions.map(|ptr| unsafe { &*ptr })
    }

    pub fn get_value_range(&self, var: &VarId) -> Option<Range> {
        let value = self.get_value(var)?;
        Some(self.range_assumptions()?.get_range(value))
    }

    pub fn bind_value(&mut self, var: VarId, value: ValueId) {
        self.values.insert(var, value);
    }

    pub fn bind_constant(&mut self, var: VarId, constant: i64) {
        self.constants.insert(var, constant);
    }

    pub fn get_value(&self, var: &VarId) -> Option<ValueId> {
        self.values.get(var).copied()
    }

    pub fn get_constant(&self, var: &VarId) -> Option<i64> {
        self.constants.get(var).copied()
    }

    pub fn dfg(&self) -> &DataFlowGraph {
        unsafe { &*self.dfg }
    }
}

/// A rewrite rule: lhs => rhs (with optional side conditions).
#[derive(Debug, Clone)]
pub struct Rewrite {
    pub name: String,
    pub lhs: Pattern,
    pub rhs: Pattern,
    pub conditions: Vec<Condition>,
}

impl Rewrite {
    pub fn new(name: impl Into<String>) -> RewriteBuilder {
        RewriteBuilder {
            name: name.into(),
            lhs: None,
            rhs: None,
            conditions: Vec::new(),
        }
    }

    pub fn check_conditions(&self, bindings: &Bindings) -> bool {
        self.conditions.iter().all(|cond| cond.check(bindings))
    }
}

pub struct RewriteBuilder {
    name: String,
    lhs: Option<Pattern>,
    rhs: Option<Pattern>,
    conditions: Vec<Condition>,
}

impl RewriteBuilder {
    pub fn match_pattern(mut self, pattern: Pattern) -> Self {
        self.lhs = Some(pattern);
        self
    }

    pub fn produce(mut self, pattern: Pattern) -> Self {
        self.rhs = Some(pattern);
        self
    }

    pub fn when(mut self, condition: Condition) -> Self {
        self.conditions.push(condition);
        self
    }

    pub fn build(self) -> Rewrite {
        Rewrite {
            name: self.name,
            lhs: self.lhs.expect("Rewrite must have LHS pattern"),
            rhs: self.rhs.expect("Rewrite must have RHS pattern"),
            conditions: self.conditions,
        }
    }
}

impl Condition {
    pub fn check(&self, bindings: &Bindings) -> bool {
        match self {
            Condition::And(conds) => conds.iter().all(|c| c.check(bindings)),

            Condition::Custom { check, .. } => check(bindings),

            Condition::InRange(var, min, max) => bindings
                .get_value_range(var)
                .map(|r| r.min >= *min && r.max <= *max)
                .unwrap_or(false),

            Condition::NonNegative(var) => bindings
                .get_value_range(var)
                .map(|r| r.is_non_negative())
                .unwrap_or(false),

            Condition::Positive(var) => bindings
                .get_value_range(var)
                .map(|r| r.is_positive())
                .unwrap_or(false),

            Condition::Negative(var) => bindings
                .get_value_range(var)
                .map(|r| r.is_negative())
                .unwrap_or(false),

            Condition::RangeLessThan(var, value) => bindings
                .get_value_range(var)
                .map(|r| r.definitely_less_than(*value))
                .unwrap_or(false),

            Condition::RangeGreaterThan(var, value) => bindings
                .get_value_range(var)
                .map(|r| r.definitely_greater_than(*value))
                .unwrap_or(false),

            Condition::RangeEquals(var, value) => bindings
                .get_value_range(var)
                .map(|r| r.definitely_equals(*value))
                .unwrap_or(false),

            Condition::Unreachable(var) => bindings
                .get_value(var)
                .and_then(|v| bindings.range_assumptions().map(|a| a.is_unreachable(v)))
                .unwrap_or(false),

            Condition::NonZero(var) => bindings
                .get_value(var)
                .and_then(|v| bindings.range_assumptions().map(|ra| ra.is_nonzero(v)))
                .unwrap_or(false),
        }
    }

    pub fn non_negative(var: impl Into<String>) -> Self {
        Condition::NonNegative(VarId::new(var))
    }

    pub fn nonzero(var: impl Into<String>) -> Self {
        Condition::NonZero(VarId::new(var))
    }
}

/// Matches patterns against values in the egraph.
pub struct PatternMatcher<'a> {
    dfg: &'a DataFlowGraph,
}

impl<'a> PatternMatcher<'a> {
    pub fn new(dfg: &'a DataFlowGraph) -> Self {
        Self { dfg }
    }

    pub fn match_pattern(&self, pattern: &Pattern, value: ValueId) -> Option<Bindings> {
        let mut bindings = Bindings::new(self.dfg);
        if self.match_pattern_impl(pattern, value, &mut bindings) {
            Some(bindings)
        } else {
            None
        }
    }

    fn match_pattern_impl(
        &self,
        pattern: &Pattern,
        value: ValueId,
        bindings: &mut Bindings,
    ) -> bool {
        match pattern {
            Pattern::Var(var) => {
                // If already bound, must match the same value.
                if let Some(bound_value) = bindings.get_value(var) {
                    bound_value == value
                } else {
                    bindings.bind_value(var.clone(), value);
                    true
                }
            }

            Pattern::Op { opcode, args } => match self.dfg.value_def(value) {
                ValueDef::Inst(inst_id) => {
                    let inst = &self.dfg.insts[&inst_id];

                    if inst.opcode != *opcode || inst.args.len() != args.len() {
                        return false;
                    }

                    for (pattern_arg, &value_arg) in args.iter().zip(&inst.args) {
                        if !self.match_pattern_impl(pattern_arg, value_arg, bindings) {
                            return false;
                        }
                    }

                    true
                }
                _ => false,
            },

            Pattern::Constant(expected) => match self.dfg.value_def(value) {
                ValueDef::Inst(inst_id) => {
                    let inst = &self.dfg.insts[&inst_id];
                    inst.opcode == Opcode::Const && inst.immediate == Some(*expected)
                }
                _ => false,
            },

            Pattern::AnyConstant(var) => match self.dfg.value_def(value) {
                ValueDef::Inst(inst_id) => {
                    let inst = &self.dfg.insts[&inst_id];
                    if inst.opcode == Opcode::Const {
                        if let Some(imm) = inst.immediate {
                            bindings.bind_value(var.clone(), value);
                            bindings.bind_constant(var.clone(), imm);
                            return true;
                        }
                    }
                    false
                }
                _ => false,
            },

            Pattern::Or(alternatives) => {
                for alt in alternatives {
                    let mut alt_bindings = bindings.clone();
                    if self.match_pattern_impl(alt, value, &mut alt_bindings) {
                        *bindings = alt_bindings;
                        return true;
                    }
                }
                false
            }
        }
    }
}

/// Builds new values from a pattern + bindings.
pub struct PatternApplier<'a> {
    dfg: &'a mut DataFlowGraph,
}

impl<'a> PatternApplier<'a> {
    pub fn new(dfg: &'a mut DataFlowGraph) -> Self {
        Self { dfg }
    }

    pub fn apply_pattern(&mut self, pattern: &Pattern, bindings: &Bindings) -> Option<ValueId> {
        match pattern {
            Pattern::Var(var) => bindings.get_value(var),

            Pattern::Op { opcode, args } => {
                let mut arg_values = Vec::new();
                for arg_pattern in args {
                    let arg_value = self.apply_pattern(arg_pattern, bindings)?;
                    arg_values.push(arg_value);
                }

                // Pull type from the first arg.
                let ty = if let Some(&first) = arg_values.first() {
                    self.dfg.value_type(first)
                } else {
                    Type::I32
                };

                let inst = Instruction::new(*opcode, arg_values, ty);
                let inst_id = self.dfg.make_inst(inst);
                Some(self.dfg.make_inst_result(inst_id, ty))
            }

            Pattern::Constant(value) => {
                let inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, *value);
                let inst_id = self.dfg.make_inst(inst);
                Some(self.dfg.make_inst_result(inst_id, Type::I32))
            }

            Pattern::AnyConstant(var) => {
                // Prefer a computed "result" constant (from const-folding side conditions);
                // fall back to the bound constant.
                let result_key = VarId::new("result");
                let const_val = if var.0 == "result" {
                    bindings.get_constant(var)
                } else {
                    bindings
                        .get_constant(&result_key)
                        .or_else(|| bindings.get_constant(var))
                };
                if let Some(val) = const_val {
                    let inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, val);
                    let inst_id = self.dfg.make_inst(inst);
                    Some(self.dfg.make_inst_result(inst_id, Type::I32))
                } else {
                    None
                }
            }

            // Doesn't make sense on the RHS.
            Pattern::Or(_) => None,
        }
    }
}

/// Standard library of common rewrite rules
pub struct RewriteLibrary {
    pub rules: Vec<Rewrite>,
}

impl RewriteLibrary {
    /// Create a library with common algebraic simplification rules
    pub fn standard() -> Self {
        let mut rules = Vec::new();

        // Arithmetic identities
        // x + 0 => x
        rules.push(
            Rewrite::new("add-zero")
                .match_pattern(Pattern::op(
                    Opcode::Add,
                    vec![Pattern::var("x"), Pattern::constant(0)],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        // 0 + x => x
        rules.push(
            Rewrite::new("add-zero-left")
                .match_pattern(Pattern::op(
                    Opcode::Add,
                    vec![Pattern::constant(0), Pattern::var("x")],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        rules.push(
            Rewrite::new("mul-one")
                .match_pattern(Pattern::op(
                    Opcode::Mul,
                    vec![Pattern::var("x"), Pattern::constant(1)],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        rules.push(
            Rewrite::new("mul-one-left")
                .match_pattern(Pattern::op(
                    Opcode::Mul,
                    vec![Pattern::constant(1), Pattern::var("x")],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        rules.push(
            Rewrite::new("add-zero-right")
                .match_pattern(Pattern::op(
                    Opcode::Add,
                    vec![Pattern::var("x"), Pattern::constant(0)],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        rules.push(
            Rewrite::new("mul-zero")
                .match_pattern(Pattern::op(
                    Opcode::Mul,
                    vec![Pattern::var("x"), Pattern::constant(0)],
                ))
                .produce(Pattern::constant(0))
                .build(),
        );

        rules.push(
            Rewrite::new("mul-zero-left")
                .match_pattern(Pattern::op(
                    Opcode::Mul,
                    vec![Pattern::constant(0), Pattern::var("x")],
                ))
                .produce(Pattern::constant(0))
                .build(),
        );

        rules.push(
            Rewrite::new("mul-one")
                .match_pattern(Pattern::op(
                    Opcode::Mul,
                    vec![Pattern::var("x"), Pattern::constant(1)],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        rules.push(
            Rewrite::new("mul-one-left")
                .match_pattern(Pattern::op(
                    Opcode::Mul,
                    vec![Pattern::constant(1), Pattern::var("x")],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        rules.push(
            Rewrite::new("sub-zero")
                .match_pattern(Pattern::op(
                    Opcode::Sub,
                    vec![Pattern::var("x"), Pattern::constant(0)],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        rules.push(
            Rewrite::new("sub-self")
                .match_pattern(Pattern::op(
                    Opcode::Sub,
                    vec![Pattern::var("x"), Pattern::var("x")],
                ))
                .produce(Pattern::constant(0))
                .build(),
        );

        // Strength reduction
        rules.push(
            Rewrite::new("mul-by-two")
                .match_pattern(Pattern::op(
                    Opcode::Mul,
                    vec![Pattern::var("x"), Pattern::constant(2)],
                ))
                .produce(Pattern::op(
                    Opcode::Add,
                    vec![Pattern::var("x"), Pattern::var("x")],
                ))
                .build(),
        );

        // Constant folding
        rules.push(
            Rewrite::new("const-fold-add")
                .match_pattern(Pattern::op(
                    Opcode::Add,
                    vec![Pattern::any_constant("a"), Pattern::any_constant("b")],
                ))
                .produce(Pattern::any_constant("result"))
                .when(Condition::Custom {
                    name: "compute-sum".to_string(),
                    check: |bindings| {
                        if let (Some(a), Some(b)) = (
                            bindings.get_constant(&VarId::new("a")),
                            bindings.get_constant(&VarId::new("b")),
                        ) {
                            unsafe {
                                let bp = bindings as *const Bindings as *mut Bindings;
                                (*bp)
                                    .constants
                                    .insert(VarId::new("result"), a.wrapping_add(b));
                            }
                            true
                        } else {
                            false
                        }
                    },
                })
                .build(),
        );

        // Commutativity
        rules.push(
            Rewrite::new("add-commute")
                .match_pattern(Pattern::op(
                    Opcode::Add,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .produce(Pattern::op(
                    Opcode::Add,
                    vec![Pattern::var("y"), Pattern::var("x")],
                ))
                .build(),
        );

        // Shift optimizations
        rules.push(
            Rewrite::new("shl-zero")
                .match_pattern(Pattern::op(
                    Opcode::Shl,
                    vec![Pattern::var("x"), Pattern::constant(0)],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        rules.push(
            Rewrite::new("ushr-zero")
                .match_pattern(Pattern::op(
                    Opcode::Ushr,
                    vec![Pattern::var("x"), Pattern::constant(0)],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        rules.push(
            Rewrite::new("sshr-zero")
                .match_pattern(Pattern::op(
                    Opcode::Sshr,
                    vec![Pattern::var("x"), Pattern::constant(0)],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        // Bitwise NOT optimizations
        rules.push(
            Rewrite::new("bnot-bnot")
                .match_pattern(Pattern::op(
                    Opcode::Bnot,
                    vec![Pattern::op(Opcode::Bnot, vec![Pattern::var("x")])],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        // Negation optimizations
        rules.push(
            Rewrite::new("ineg-ineg")
                .match_pattern(Pattern::op(
                    Opcode::Ineg,
                    vec![Pattern::op(Opcode::Ineg, vec![Pattern::var("x")])],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        rules.push(
            Rewrite::new("ineg-zero")
                .match_pattern(Pattern::op(Opcode::Ineg, vec![Pattern::constant(0)]))
                .produce(Pattern::constant(0))
                .build(),
        );

        // Comparison simplifications
        rules.push(
            Rewrite::new("eq-self")
                .match_pattern(Pattern::op(
                    Opcode::Eq,
                    vec![Pattern::var("x"), Pattern::var("x")],
                ))
                .produce(Pattern::constant(1))
                .build(),
        );

        rules.push(
            Rewrite::new("ne-self")
                .match_pattern(Pattern::op(
                    Opcode::Ne,
                    vec![Pattern::var("x"), Pattern::var("x")],
                ))
                .produce(Pattern::constant(0))
                .build(),
        );

        rules.push(
            Rewrite::new("slt-self")
                .match_pattern(Pattern::op(
                    Opcode::Slt,
                    vec![Pattern::var("x"), Pattern::var("x")],
                ))
                .produce(Pattern::constant(0))
                .build(),
        );

        rules.push(
            Rewrite::new("sle-self")
                .match_pattern(Pattern::op(
                    Opcode::Sle,
                    vec![Pattern::var("x"), Pattern::var("x")],
                ))
                .produce(Pattern::constant(1))
                .build(),
        );

        // Bitwise optimizations
        rules.push(
            Rewrite::new("and-self")
                .match_pattern(Pattern::op(
                    Opcode::And,
                    vec![Pattern::var("x"), Pattern::var("x")],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        rules.push(
            Rewrite::new("or-self")
                .match_pattern(Pattern::op(
                    Opcode::Or,
                    vec![Pattern::var("x"), Pattern::var("x")],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        rules.push(
            Rewrite::new("xor-self")
                .match_pattern(Pattern::op(
                    Opcode::Xor,
                    vec![Pattern::var("x"), Pattern::var("x")],
                ))
                .produce(Pattern::constant(0))
                .build(),
        );

        rules.push(
            Rewrite::new("and-zero")
                .match_pattern(Pattern::op(
                    Opcode::And,
                    vec![Pattern::var("x"), Pattern::constant(0)],
                ))
                .produce(Pattern::constant(0))
                .build(),
        );

        rules.push(
            Rewrite::new("and-zero-left")
                .match_pattern(Pattern::op(
                    Opcode::And,
                    vec![Pattern::constant(0), Pattern::var("x")],
                ))
                .produce(Pattern::constant(0))
                .build(),
        );

        rules.push(
            Rewrite::new("and-all-ones")
                .match_pattern(Pattern::op(
                    Opcode::And,
                    vec![Pattern::var("x"), Pattern::constant(-1)],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        rules.push(
            Rewrite::new("and-all-ones-left")
                .match_pattern(Pattern::op(
                    Opcode::And,
                    vec![Pattern::constant(-1), Pattern::var("x")],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        rules.push(
            Rewrite::new("or-zero")
                .match_pattern(Pattern::op(
                    Opcode::Or,
                    vec![Pattern::var("x"), Pattern::constant(0)],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        rules.push(
            Rewrite::new("or-zero-left")
                .match_pattern(Pattern::op(
                    Opcode::Or,
                    vec![Pattern::constant(0), Pattern::var("x")],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        rules.push(
            Rewrite::new("or-neg-one")
                .match_pattern(Pattern::op(
                    Opcode::Or,
                    vec![Pattern::var("x"), Pattern::constant(-1)],
                ))
                .produce(Pattern::constant(-1))
                .build(),
        );

        rules.push(
            Rewrite::new("or-neg-one-left")
                .match_pattern(Pattern::op(
                    Opcode::Or,
                    vec![Pattern::constant(-1), Pattern::var("x")],
                ))
                .produce(Pattern::constant(-1))
                .build(),
        );

        rules.push(
            Rewrite::new("xor-zero")
                .match_pattern(Pattern::op(
                    Opcode::Xor,
                    vec![Pattern::var("x"), Pattern::constant(0)],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        rules.push(
            Rewrite::new("xor-zero-left")
                .match_pattern(Pattern::op(
                    Opcode::Xor,
                    vec![Pattern::constant(0), Pattern::var("x")],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        // Zero subtracted from zero
        rules.push(
            Rewrite::new("zero-sub-zero")
                .match_pattern(Pattern::op(
                    Opcode::Sub,
                    vec![Pattern::constant(0), Pattern::constant(0)],
                ))
                .produce(Pattern::constant(0))
                .build(),
        );

        // Additional comparison patterns
        rules.push(
            Rewrite::new("sgt-self")
                .match_pattern(Pattern::op(
                    Opcode::Sgt,
                    vec![Pattern::var("x"), Pattern::var("x")],
                ))
                .produce(Pattern::constant(0))
                .build(),
        );

        rules.push(
            Rewrite::new("sge-self")
                .match_pattern(Pattern::op(
                    Opcode::Sge,
                    vec![Pattern::var("x"), Pattern::var("x")],
                ))
                .produce(Pattern::constant(1))
                .build(),
        );

        rules.push(
            Rewrite::new("ult-self")
                .match_pattern(Pattern::op(
                    Opcode::Ult,
                    vec![Pattern::var("x"), Pattern::var("x")],
                ))
                .produce(Pattern::constant(0))
                .build(),
        );

        rules.push(
            Rewrite::new("ule-self")
                .match_pattern(Pattern::op(
                    Opcode::Ule,
                    vec![Pattern::var("x"), Pattern::var("x")],
                ))
                .produce(Pattern::constant(1))
                .build(),
        );

        rules.push(
            Rewrite::new("ugt-self")
                .match_pattern(Pattern::op(
                    Opcode::Ugt,
                    vec![Pattern::var("x"), Pattern::var("x")],
                ))
                .produce(Pattern::constant(0))
                .build(),
        );

        rules.push(
            Rewrite::new("uge-self")
                .match_pattern(Pattern::op(
                    Opcode::Uge,
                    vec![Pattern::var("x"), Pattern::var("x")],
                ))
                .produce(Pattern::constant(1))
                .build(),
        );

        // Commutative operations
        rules.push(
            Rewrite::new("mul-commute")
                .match_pattern(Pattern::op(
                    Opcode::Mul,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .produce(Pattern::op(
                    Opcode::Mul,
                    vec![Pattern::var("y"), Pattern::var("x")],
                ))
                .build(),
        );

        rules.push(
            Rewrite::new("and-commute")
                .match_pattern(Pattern::op(
                    Opcode::And,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .produce(Pattern::op(
                    Opcode::And,
                    vec![Pattern::var("y"), Pattern::var("x")],
                ))
                .build(),
        );

        rules.push(
            Rewrite::new("or-commute")
                .match_pattern(Pattern::op(
                    Opcode::Or,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .produce(Pattern::op(
                    Opcode::Or,
                    vec![Pattern::var("y"), Pattern::var("x")],
                ))
                .build(),
        );

        rules.push(
            Rewrite::new("xor-commute")
                .match_pattern(Pattern::op(
                    Opcode::Xor,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .produce(Pattern::op(
                    Opcode::Xor,
                    vec![Pattern::var("y"), Pattern::var("x")],
                ))
                .build(),
        );

        // Strength reduction: shift by constant -> multiply/divide
        rules.push(
            Rewrite::new("shl-by-one")
                .match_pattern(Pattern::op(
                    Opcode::Shl,
                    vec![Pattern::var("x"), Pattern::constant(1)],
                ))
                .produce(Pattern::op(
                    Opcode::Add,
                    vec![Pattern::var("x"), Pattern::var("x")],
                ))
                .build(),
        );

        // ===== Range-sensitive rewrite rules =====
        // These rules only fire when range analysis can prove preconditions.

        // Signed division by power-of-2 -> unsigned right shift
        // when x is known non-negative: sdiv(x, 2) => ushr(x, 1)
        rules.push(
            Rewrite::new("range-sdiv-pow2-to-ushr-1")
                .match_pattern(Pattern::op(
                    Opcode::Sdiv,
                    vec![Pattern::var("x"), Pattern::constant(2)],
                ))
                .produce(Pattern::op(
                    Opcode::Ushr,
                    vec![Pattern::var("x"), Pattern::constant(1)],
                ))
                .when(Condition::non_negative("x"))
                .build(),
        );

        rules.push(
            Rewrite::new("range-sdiv-pow2-to-ushr-2")
                .match_pattern(Pattern::op(
                    Opcode::Sdiv,
                    vec![Pattern::var("x"), Pattern::constant(4)],
                ))
                .produce(Pattern::op(
                    Opcode::Ushr,
                    vec![Pattern::var("x"), Pattern::constant(2)],
                ))
                .when(Condition::non_negative("x"))
                .build(),
        );

        rules.push(
            Rewrite::new("range-sdiv-pow2-to-ushr-3")
                .match_pattern(Pattern::op(
                    Opcode::Sdiv,
                    vec![Pattern::var("x"), Pattern::constant(8)],
                ))
                .produce(Pattern::op(
                    Opcode::Ushr,
                    vec![Pattern::var("x"), Pattern::constant(3)],
                ))
                .when(Condition::non_negative("x"))
                .build(),
        );

        // Known-true comparison elimination: slt(x, C) => 1
        // when range proves x is definitely less than C.
        // (These require range_lt/range_gt conditions.)
        rules.push(
            Rewrite::new("range-fold-slt-true")
                .match_pattern(Pattern::op(
                    Opcode::Slt,
                    vec![Pattern::var("x"), Pattern::any_constant("c")],
                ))
                .produce(Pattern::constant(1))
                .when(Condition::Custom {
                    name: "x-range-lt-c".to_string(),
                    check: |bindings| {
                        let c = match bindings.get_constant(&VarId::new("c")) {
                            Some(c) => c,
                            None => return false,
                        };
                        bindings
                            .get_value_range(&VarId::new("x"))
                            .map(|r| r.definitely_less_than(c))
                            .unwrap_or(false)
                    },
                })
                .build(),
        );

        // Known-false comparison elimination: slt(x, C) => 0
        // when range proves x >= C.
        rules.push(
            Rewrite::new("range-fold-slt-false")
                .match_pattern(Pattern::op(
                    Opcode::Slt,
                    vec![Pattern::var("x"), Pattern::any_constant("c")],
                ))
                .produce(Pattern::constant(0))
                .when(Condition::Custom {
                    name: "x-range-ge-c".to_string(),
                    check: |bindings| {
                        let c = match bindings.get_constant(&VarId::new("c")) {
                            Some(c) => c,
                            None => return false,
                        };
                        bindings
                            .get_value_range(&VarId::new("x"))
                            .map(|r| r.definitely_ge(c))
                            .unwrap_or(false)
                    },
                })
                .build(),
        );

        // Known-true sle: sle(x, C) => 1 when x.max <= C
        rules.push(
            Rewrite::new("range-fold-sle-true")
                .match_pattern(Pattern::op(
                    Opcode::Sle,
                    vec![Pattern::var("x"), Pattern::any_constant("c")],
                ))
                .produce(Pattern::constant(1))
                .when(Condition::Custom {
                    name: "x-range-le-c".to_string(),
                    check: |bindings| {
                        let c = match bindings.get_constant(&VarId::new("c")) {
                            Some(c) => c,
                            None => return false,
                        };
                        bindings
                            .get_value_range(&VarId::new("x"))
                            .map(|r| r.definitely_le(c))
                            .unwrap_or(false)
                    },
                })
                .build(),
        );

        // Known-false sle: sle(x, C) => 0 when x.min > C
        rules.push(
            Rewrite::new("range-fold-sle-false")
                .match_pattern(Pattern::op(
                    Opcode::Sle,
                    vec![Pattern::var("x"), Pattern::any_constant("c")],
                ))
                .produce(Pattern::constant(0))
                .when(Condition::Custom {
                    name: "x-range-gt-c".to_string(),
                    check: |bindings| {
                        let c = match bindings.get_constant(&VarId::new("c")) {
                            Some(c) => c,
                            None => return false,
                        };
                        bindings
                            .get_value_range(&VarId::new("x"))
                            .map(|r| r.definitely_greater_than(c))
                            .unwrap_or(false)
                    },
                })
                .build(),
        );

        // Known-true sge: sge(x, C) => 1 when x.min >= C
        rules.push(
            Rewrite::new("range-fold-sge-true")
                .match_pattern(Pattern::op(
                    Opcode::Sge,
                    vec![Pattern::var("x"), Pattern::any_constant("c")],
                ))
                .produce(Pattern::constant(1))
                .when(Condition::Custom {
                    name: "x-range-ge-c".to_string(),
                    check: |bindings| {
                        let c = match bindings.get_constant(&VarId::new("c")) {
                            Some(c) => c,
                            None => return false,
                        };
                        bindings
                            .get_value_range(&VarId::new("x"))
                            .map(|r| r.definitely_ge(c))
                            .unwrap_or(false)
                    },
                })
                .build(),
        );

        // Known-false sge: sge(x, C) => 0 when x.max < C
        rules.push(
            Rewrite::new("range-fold-sge-false")
                .match_pattern(Pattern::op(
                    Opcode::Sge,
                    vec![Pattern::var("x"), Pattern::any_constant("c")],
                ))
                .produce(Pattern::constant(0))
                .when(Condition::Custom {
                    name: "x-range-lt-c".to_string(),
                    check: |bindings| {
                        let c = match bindings.get_constant(&VarId::new("c")) {
                            Some(c) => c,
                            None => return false,
                        };
                        bindings
                            .get_value_range(&VarId::new("x"))
                            .map(|r| r.definitely_less_than(c))
                            .unwrap_or(false)
                    },
                })
                .build(),
        );

        // Known-true sgt: sgt(x, C) => 1 when x.min > C
        rules.push(
            Rewrite::new("range-fold-sgt-true")
                .match_pattern(Pattern::op(
                    Opcode::Sgt,
                    vec![Pattern::var("x"), Pattern::any_constant("c")],
                ))
                .produce(Pattern::constant(1))
                .when(Condition::Custom {
                    name: "x-range-gt-c".to_string(),
                    check: |bindings| {
                        let c = match bindings.get_constant(&VarId::new("c")) {
                            Some(c) => c,
                            None => return false,
                        };
                        bindings
                            .get_value_range(&VarId::new("x"))
                            .map(|r| r.definitely_greater_than(c))
                            .unwrap_or(false)
                    },
                })
                .build(),
        );

        // Known-false sgt: sgt(x, C) => 0 when x.max <= C
        rules.push(
            Rewrite::new("range-fold-sgt-false")
                .match_pattern(Pattern::op(
                    Opcode::Sgt,
                    vec![Pattern::var("x"), Pattern::any_constant("c")],
                ))
                .produce(Pattern::constant(0))
                .when(Condition::Custom {
                    name: "x-range-le-c".to_string(),
                    check: |bindings| {
                        let c = match bindings.get_constant(&VarId::new("c")) {
                            Some(c) => c,
                            None => return false,
                        };
                        bindings
                            .get_value_range(&VarId::new("x"))
                            .map(|r| r.definitely_le(c))
                            .unwrap_or(false)
                    },
                })
                .build(),
        );

        // Range-based constant propagation: if range is a singleton, replace with constant
        // This is handled implicitly by compute_inst_range + the egraph,
        // but we add explicit rules for key patterns:

        // Redundant comparison after bounds check:
        // If x is in [0, C-1] (from a branch condition), then slt(x, C) is always true
        // This is already covered by range-fold-slt-true above.

        // Non-negative x: sub(0, sub(0, x)) => x (double negate identity, but
        // with the knowledge that abs(x) = x when x >= 0, we can simplify)

        // ===== ISLE-derived Arithmetic Rules =====

        // ineg(x) => sub(0, x)
        rules.push(
            Rewrite::new("ineg-to-sub")
                .match_pattern(Pattern::op(Opcode::Ineg, vec![Pattern::var("x")]))
                .produce(Pattern::op(
                    Opcode::Sub,
                    vec![Pattern::constant(0), Pattern::var("x")],
                ))
                .build(),
        );

        // sub(0, x) => ineg(x)
        rules.push(
            Rewrite::new("sub-zero-to-ineg")
                .match_pattern(Pattern::op(
                    Opcode::Sub,
                    vec![Pattern::constant(0), Pattern::var("x")],
                ))
                .produce(Pattern::op(Opcode::Ineg, vec![Pattern::var("x")]))
                .build(),
        );

        // iabs(ineg(x)) => iabs(x)
        rules.push(
            Rewrite::new("iabs-ineg")
                .match_pattern(Pattern::op(
                    Opcode::Iabs,
                    vec![Pattern::op(Opcode::Ineg, vec![Pattern::var("x")])],
                ))
                .produce(Pattern::op(Opcode::Iabs, vec![Pattern::var("x")]))
                .build(),
        );

        // iabs(iabs(x)) => iabs(x)
        rules.push(
            Rewrite::new("iabs-iabs")
                .match_pattern(Pattern::op(
                    Opcode::Iabs,
                    vec![Pattern::op(Opcode::Iabs, vec![Pattern::var("x")])],
                ))
                .produce(Pattern::op(Opcode::Iabs, vec![Pattern::var("x")]))
                .build(),
        );

        // iabs(x) => x when x is non-negative
        rules.push(
            Rewrite::new("iabs-non-negative")
                .match_pattern(Pattern::op(Opcode::Iabs, vec![Pattern::var("x")]))
                .produce(Pattern::var("x"))
                .when(Condition::non_negative("x"))
                .build(),
        );

        // mul(x, power-of-2) => shl(x, log2)  for 4, 8, 16, 32, 64
        for (pow, shift) in [(4i64, 2i64), (8, 3), (16, 4), (32, 5), (64, 6)] {
            rules.push(
                Rewrite::new(format!("mul-pow2-{}", pow))
                    .match_pattern(Pattern::op(
                        Opcode::Mul,
                        vec![Pattern::var("x"), Pattern::constant(pow)],
                    ))
                    .produce(Pattern::op(
                        Opcode::Shl,
                        vec![Pattern::var("x"), Pattern::constant(shift)],
                    ))
                    .build(),
            );
        }

        // add(x, x) => shl(x, 1)
        rules.push(
            Rewrite::new("add-self-to-shl")
                .match_pattern(Pattern::op(
                    Opcode::Add,
                    vec![Pattern::var("x"), Pattern::var("x")],
                ))
                .produce(Pattern::op(
                    Opcode::Shl,
                    vec![Pattern::var("x"), Pattern::constant(1)],
                ))
                .build(),
        );

        // add(x, ineg(y)) => sub(x, y)
        rules.push(
            Rewrite::new("add-ineg-to-sub")
                .match_pattern(Pattern::op(
                    Opcode::Add,
                    vec![
                        Pattern::var("x"),
                        Pattern::op(Opcode::Ineg, vec![Pattern::var("y")]),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Sub,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .build(),
        );

        // sub(x, ineg(y)) => add(x, y)
        rules.push(
            Rewrite::new("sub-ineg-to-add")
                .match_pattern(Pattern::op(
                    Opcode::Sub,
                    vec![
                        Pattern::var("x"),
                        Pattern::op(Opcode::Ineg, vec![Pattern::var("y")]),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Add,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .build(),
        );

        // add(sub(x, y), y) => x
        rules.push(
            Rewrite::new("add-sub-cancel")
                .match_pattern(Pattern::op(
                    Opcode::Add,
                    vec![
                        Pattern::op(Opcode::Sub, vec![Pattern::var("x"), Pattern::var("y")]),
                        Pattern::var("y"),
                    ],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        // sub(add(x, y), y) => x
        rules.push(
            Rewrite::new("sub-add-cancel")
                .match_pattern(Pattern::op(
                    Opcode::Sub,
                    vec![
                        Pattern::op(Opcode::Add, vec![Pattern::var("x"), Pattern::var("y")]),
                        Pattern::var("y"),
                    ],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        // sub(add(x, y), x) => y
        rules.push(
            Rewrite::new("sub-add-cancel-2")
                .match_pattern(Pattern::op(
                    Opcode::Sub,
                    vec![
                        Pattern::op(Opcode::Add, vec![Pattern::var("x"), Pattern::var("y")]),
                        Pattern::var("x"),
                    ],
                ))
                .produce(Pattern::var("y"))
                .build(),
        );

        // bnot(x) + 1 = ineg(x)  i.e. add(bnot(x), 1) => ineg(x)
        rules.push(
            Rewrite::new("bnot-plus-one-to-ineg")
                .match_pattern(Pattern::op(
                    Opcode::Add,
                    vec![
                        Pattern::op(Opcode::Bnot, vec![Pattern::var("x")]),
                        Pattern::constant(1),
                    ],
                ))
                .produce(Pattern::op(Opcode::Ineg, vec![Pattern::var("x")]))
                .build(),
        );

        // add(bnot(x), x) => -1
        rules.push(
            Rewrite::new("bnot-plus-x")
                .match_pattern(Pattern::op(
                    Opcode::Add,
                    vec![
                        Pattern::op(Opcode::Bnot, vec![Pattern::var("x")]),
                        Pattern::var("x"),
                    ],
                ))
                .produce(Pattern::constant(-1))
                .build(),
        );

        // sdiv(x, 1) => x
        rules.push(
            Rewrite::new("sdiv-one")
                .match_pattern(Pattern::op(
                    Opcode::Sdiv,
                    vec![Pattern::var("x"), Pattern::constant(1)],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        // udiv(x, 1) => x
        rules.push(
            Rewrite::new("udiv-one")
                .match_pattern(Pattern::op(
                    Opcode::Udiv,
                    vec![Pattern::var("x"), Pattern::constant(1)],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        // urem(x, 1) => 0
        rules.push(
            Rewrite::new("urem-one")
                .match_pattern(Pattern::op(
                    Opcode::Urem,
                    vec![Pattern::var("x"), Pattern::constant(1)],
                ))
                .produce(Pattern::constant(0))
                .build(),
        );

        // srem(x, 1) => 0
        rules.push(
            Rewrite::new("srem-one")
                .match_pattern(Pattern::op(
                    Opcode::Srem,
                    vec![Pattern::var("x"), Pattern::constant(1)],
                ))
                .produce(Pattern::constant(0))
                .build(),
        );

        // sdiv(0, x) => 0
        rules.push(
            Rewrite::new("sdiv-zero-num")
                .match_pattern(Pattern::op(
                    Opcode::Sdiv,
                    vec![Pattern::constant(0), Pattern::var("x")],
                ))
                .produce(Pattern::constant(0))
                .build(),
        );

        // udiv(0, x) => 0
        rules.push(
            Rewrite::new("udiv-zero-num")
                .match_pattern(Pattern::op(
                    Opcode::Udiv,
                    vec![Pattern::constant(0), Pattern::var("x")],
                ))
                .produce(Pattern::constant(0))
                .build(),
        );

        // srem(0, x) => 0
        rules.push(
            Rewrite::new("srem-zero-num")
                .match_pattern(Pattern::op(
                    Opcode::Srem,
                    vec![Pattern::constant(0), Pattern::var("x")],
                ))
                .produce(Pattern::constant(0))
                .build(),
        );

        // urem(0, x) => 0
        rules.push(
            Rewrite::new("urem-zero-num")
                .match_pattern(Pattern::op(
                    Opcode::Urem,
                    vec![Pattern::constant(0), Pattern::var("x")],
                ))
                .produce(Pattern::constant(0))
                .build(),
        );

        // sdiv(x, -1) => ineg(x)
        rules.push(
            Rewrite::new("sdiv-neg-one")
                .match_pattern(Pattern::op(
                    Opcode::Sdiv,
                    vec![Pattern::var("x"), Pattern::constant(-1)],
                ))
                .produce(Pattern::op(Opcode::Ineg, vec![Pattern::var("x")]))
                .build(),
        );

        // srem(x, -1) => 0
        rules.push(
            Rewrite::new("srem-neg-one")
                .match_pattern(Pattern::op(
                    Opcode::Srem,
                    vec![Pattern::var("x"), Pattern::constant(-1)],
                ))
                .produce(Pattern::constant(0))
                .build(),
        );

        // select(1, x, y) => x
        rules.push(
            Rewrite::new("select-true")
                .match_pattern(Pattern::op(
                    Opcode::Select,
                    vec![Pattern::constant(1), Pattern::var("x"), Pattern::var("y")],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        // select(0, x, y) => y
        rules.push(
            Rewrite::new("select-false")
                .match_pattern(Pattern::op(
                    Opcode::Select,
                    vec![Pattern::constant(0), Pattern::var("x"), Pattern::var("y")],
                ))
                .produce(Pattern::var("y"))
                .build(),
        );

        // select(c, x, x) => x
        rules.push(
            Rewrite::new("select-same")
                .match_pattern(Pattern::op(
                    Opcode::Select,
                    vec![Pattern::var("c"), Pattern::var("x"), Pattern::var("x")],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        // ===== ISLE-derived Bitwise Rules =====

        // (x & y) | (x & z) => x & (y | z)   (factor-and)
        rules.push(
            Rewrite::new("factor-and")
                .match_pattern(Pattern::op(
                    Opcode::Or,
                    vec![
                        Pattern::op(Opcode::And, vec![Pattern::var("x"), Pattern::var("y")]),
                        Pattern::op(Opcode::And, vec![Pattern::var("x"), Pattern::var("z")]),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::And,
                    vec![
                        Pattern::var("x"),
                        Pattern::op(Opcode::Or, vec![Pattern::var("y"), Pattern::var("z")]),
                    ],
                ))
                .build(),
        );

        // (x | y) & (x | z) => x | (y & z)   (factor-or)
        rules.push(
            Rewrite::new("factor-or")
                .match_pattern(Pattern::op(
                    Opcode::And,
                    vec![
                        Pattern::op(Opcode::Or, vec![Pattern::var("x"), Pattern::var("y")]),
                        Pattern::op(Opcode::Or, vec![Pattern::var("x"), Pattern::var("z")]),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Or,
                    vec![
                        Pattern::var("x"),
                        Pattern::op(Opcode::And, vec![Pattern::var("y"), Pattern::var("z")]),
                    ],
                ))
                .build(),
        );

        // xor(x, -1) => bnot(x)
        rules.push(
            Rewrite::new("xor-neg1-to-bnot")
                .match_pattern(Pattern::op(
                    Opcode::Xor,
                    vec![Pattern::var("x"), Pattern::constant(-1)],
                ))
                .produce(Pattern::op(Opcode::Bnot, vec![Pattern::var("x")]))
                .build(),
        );

        // bnot(x) => xor(x, -1)
        rules.push(
            Rewrite::new("bnot-to-xor-neg1")
                .match_pattern(Pattern::op(Opcode::Bnot, vec![Pattern::var("x")]))
                .produce(Pattern::op(
                    Opcode::Xor,
                    vec![Pattern::var("x"), Pattern::constant(-1)],
                ))
                .build(),
        );

        // and(x, or(x, y)) => x   (absorption)
        rules.push(
            Rewrite::new("and-absorb")
                .match_pattern(Pattern::op(
                    Opcode::And,
                    vec![
                        Pattern::var("x"),
                        Pattern::op(Opcode::Or, vec![Pattern::var("x"), Pattern::var("y")]),
                    ],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        // or(x, and(x, y)) => x   (absorption)
        rules.push(
            Rewrite::new("or-absorb")
                .match_pattern(Pattern::op(
                    Opcode::Or,
                    vec![
                        Pattern::var("x"),
                        Pattern::op(Opcode::And, vec![Pattern::var("x"), Pattern::var("y")]),
                    ],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        // and(x, bnot(x)) => 0
        rules.push(
            Rewrite::new("and-bnot-zero")
                .match_pattern(Pattern::op(
                    Opcode::And,
                    vec![
                        Pattern::var("x"),
                        Pattern::op(Opcode::Bnot, vec![Pattern::var("x")]),
                    ],
                ))
                .produce(Pattern::constant(0))
                .build(),
        );

        // or(x, bnot(x)) => -1
        rules.push(
            Rewrite::new("or-bnot-all-ones")
                .match_pattern(Pattern::op(
                    Opcode::Or,
                    vec![
                        Pattern::var("x"),
                        Pattern::op(Opcode::Bnot, vec![Pattern::var("x")]),
                    ],
                ))
                .produce(Pattern::constant(-1))
                .build(),
        );

        // xor(x, bnot(x)) => -1
        rules.push(
            Rewrite::new("xor-bnot-all-ones")
                .match_pattern(Pattern::op(
                    Opcode::Xor,
                    vec![
                        Pattern::var("x"),
                        Pattern::op(Opcode::Bnot, vec![Pattern::var("x")]),
                    ],
                ))
                .produce(Pattern::constant(-1))
                .build(),
        );

        // (x & y) ^ (x ^ y) => x | y   (ISLE: bxor_bor via De Morgan)
        rules.push(
            Rewrite::new("and-xor-to-or")
                .match_pattern(Pattern::op(
                    Opcode::Xor,
                    vec![
                        Pattern::op(Opcode::And, vec![Pattern::var("x"), Pattern::var("y")]),
                        Pattern::op(Opcode::Xor, vec![Pattern::var("x"), Pattern::var("y")]),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Or,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .build(),
        );

        // (x | y) ^ (x ^ y) => x & y   (complement of above)
        rules.push(
            Rewrite::new("or-xor-to-and")
                .match_pattern(Pattern::op(
                    Opcode::Xor,
                    vec![
                        Pattern::op(Opcode::Or, vec![Pattern::var("x"), Pattern::var("y")]),
                        Pattern::op(Opcode::Xor, vec![Pattern::var("x"), Pattern::var("y")]),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::And,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .build(),
        );

        // bnot(and(x, y)) => or(bnot(x), bnot(y))  (De Morgan)
        rules.push(
            Rewrite::new("demorgan-and")
                .match_pattern(Pattern::op(
                    Opcode::Bnot,
                    vec![Pattern::op(
                        Opcode::And,
                        vec![Pattern::var("x"), Pattern::var("y")],
                    )],
                ))
                .produce(Pattern::op(
                    Opcode::Or,
                    vec![
                        Pattern::op(Opcode::Bnot, vec![Pattern::var("x")]),
                        Pattern::op(Opcode::Bnot, vec![Pattern::var("y")]),
                    ],
                ))
                .build(),
        );

        // bnot(or(x, y)) => and(bnot(x), bnot(y))  (De Morgan)
        rules.push(
            Rewrite::new("demorgan-or")
                .match_pattern(Pattern::op(
                    Opcode::Bnot,
                    vec![Pattern::op(
                        Opcode::Or,
                        vec![Pattern::var("x"), Pattern::var("y")],
                    )],
                ))
                .produce(Pattern::op(
                    Opcode::And,
                    vec![
                        Pattern::op(Opcode::Bnot, vec![Pattern::var("x")]),
                        Pattern::op(Opcode::Bnot, vec![Pattern::var("y")]),
                    ],
                ))
                .build(),
        );

        // rotl(x, 0) => x
        rules.push(
            Rewrite::new("rotl-zero")
                .match_pattern(Pattern::op(
                    Opcode::Rotl,
                    vec![Pattern::var("x"), Pattern::constant(0)],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        // rotr(x, 0) => x
        rules.push(
            Rewrite::new("rotr-zero")
                .match_pattern(Pattern::op(
                    Opcode::Rotr,
                    vec![Pattern::var("x"), Pattern::constant(0)],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        // ===== ISLE-derived Constant Folding Rules =====

        // const-fold-add: already exists above but doesn't compute result.
        // Fix the existing rule by replacing it with a working version.
        // We add properly-computing versions for all ops below.

        // const-fold-sub: sub(a, b) => a - b
        rules.push(
            Rewrite::new("const-fold-sub")
                .match_pattern(Pattern::op(
                    Opcode::Sub,
                    vec![Pattern::any_constant("a"), Pattern::any_constant("b")],
                ))
                .produce(Pattern::any_constant("result"))
                .when(Condition::Custom {
                    name: "compute-sub".to_string(),
                    check: |bindings| {
                        if let (Some(a), Some(b)) = (
                            bindings.get_constant(&VarId::new("a")),
                            bindings.get_constant(&VarId::new("b")),
                        ) {
                            unsafe {
                                let bp = bindings as *const Bindings as *mut Bindings;
                                (*bp)
                                    .constants
                                    .insert(VarId::new("result"), a.wrapping_sub(b));
                            }
                            true
                        } else {
                            false
                        }
                    },
                })
                .build(),
        );

        // const-fold-mul: mul(a, b) => a * b
        rules.push(
            Rewrite::new("const-fold-mul")
                .match_pattern(Pattern::op(
                    Opcode::Mul,
                    vec![Pattern::any_constant("a"), Pattern::any_constant("b")],
                ))
                .produce(Pattern::any_constant("result"))
                .when(Condition::Custom {
                    name: "compute-mul".to_string(),
                    check: |bindings| {
                        if let (Some(a), Some(b)) = (
                            bindings.get_constant(&VarId::new("a")),
                            bindings.get_constant(&VarId::new("b")),
                        ) {
                            unsafe {
                                let bp = bindings as *const Bindings as *mut Bindings;
                                (*bp)
                                    .constants
                                    .insert(VarId::new("result"), a.wrapping_mul(b));
                            }
                            true
                        } else {
                            false
                        }
                    },
                })
                .build(),
        );

        // const-fold-and: and(a, b) => a & b
        rules.push(
            Rewrite::new("const-fold-and")
                .match_pattern(Pattern::op(
                    Opcode::And,
                    vec![Pattern::any_constant("a"), Pattern::any_constant("b")],
                ))
                .produce(Pattern::any_constant("result"))
                .when(Condition::Custom {
                    name: "compute-and".to_string(),
                    check: |bindings| {
                        if let (Some(a), Some(b)) = (
                            bindings.get_constant(&VarId::new("a")),
                            bindings.get_constant(&VarId::new("b")),
                        ) {
                            unsafe {
                                let bp = bindings as *const Bindings as *mut Bindings;
                                (*bp).constants.insert(VarId::new("result"), a & b);
                            }
                            true
                        } else {
                            false
                        }
                    },
                })
                .build(),
        );

        // const-fold-or: or(a, b) => a | b
        rules.push(
            Rewrite::new("const-fold-or")
                .match_pattern(Pattern::op(
                    Opcode::Or,
                    vec![Pattern::any_constant("a"), Pattern::any_constant("b")],
                ))
                .produce(Pattern::any_constant("result"))
                .when(Condition::Custom {
                    name: "compute-or".to_string(),
                    check: |bindings| {
                        if let (Some(a), Some(b)) = (
                            bindings.get_constant(&VarId::new("a")),
                            bindings.get_constant(&VarId::new("b")),
                        ) {
                            unsafe {
                                let bp = bindings as *const Bindings as *mut Bindings;
                                (*bp).constants.insert(VarId::new("result"), a | b);
                            }
                            true
                        } else {
                            false
                        }
                    },
                })
                .build(),
        );

        // const-fold-xor: xor(a, b) => a ^ b
        rules.push(
            Rewrite::new("const-fold-xor")
                .match_pattern(Pattern::op(
                    Opcode::Xor,
                    vec![Pattern::any_constant("a"), Pattern::any_constant("b")],
                ))
                .produce(Pattern::any_constant("result"))
                .when(Condition::Custom {
                    name: "compute-xor".to_string(),
                    check: |bindings| {
                        if let (Some(a), Some(b)) = (
                            bindings.get_constant(&VarId::new("a")),
                            bindings.get_constant(&VarId::new("b")),
                        ) {
                            unsafe {
                                let bp = bindings as *const Bindings as *mut Bindings;
                                (*bp).constants.insert(VarId::new("result"), a ^ b);
                            }
                            true
                        } else {
                            false
                        }
                    },
                })
                .build(),
        );

        // const-fold-shl: shl(a, b) => a << b
        rules.push(
            Rewrite::new("const-fold-shl")
                .match_pattern(Pattern::op(
                    Opcode::Shl,
                    vec![Pattern::any_constant("a"), Pattern::any_constant("b")],
                ))
                .produce(Pattern::any_constant("result"))
                .when(Condition::Custom {
                    name: "compute-shl".to_string(),
                    check: |bindings| {
                        if let (Some(a), Some(b)) = (
                            bindings.get_constant(&VarId::new("a")),
                            bindings.get_constant(&VarId::new("b")),
                        ) {
                            if b >= 0 && b < 64 {
                                unsafe {
                                    let bp = bindings as *const Bindings as *mut Bindings;
                                    (*bp)
                                        .constants
                                        .insert(VarId::new("result"), a.wrapping_shl(b as u32));
                                }
                                true
                            } else {
                                false
                            }
                        } else {
                            false
                        }
                    },
                })
                .build(),
        );

        // const-fold-ushr: ushr(a, b) => (a as u64) >> b
        rules.push(
            Rewrite::new("const-fold-ushr")
                .match_pattern(Pattern::op(
                    Opcode::Ushr,
                    vec![Pattern::any_constant("a"), Pattern::any_constant("b")],
                ))
                .produce(Pattern::any_constant("result"))
                .when(Condition::Custom {
                    name: "compute-ushr".to_string(),
                    check: |bindings| {
                        if let (Some(a), Some(b)) = (
                            bindings.get_constant(&VarId::new("a")),
                            bindings.get_constant(&VarId::new("b")),
                        ) {
                            if b >= 0 && b < 64 {
                                unsafe {
                                    let bp = bindings as *const Bindings as *mut Bindings;
                                    (*bp).constants.insert(
                                        VarId::new("result"),
                                        ((a as u64).wrapping_shr(b as u32)) as i64,
                                    );
                                }
                                true
                            } else {
                                false
                            }
                        } else {
                            false
                        }
                    },
                })
                .build(),
        );

        // const-fold-sshr: sshr(a, b) => a >> b (arithmetic)
        rules.push(
            Rewrite::new("const-fold-sshr")
                .match_pattern(Pattern::op(
                    Opcode::Sshr,
                    vec![Pattern::any_constant("a"), Pattern::any_constant("b")],
                ))
                .produce(Pattern::any_constant("result"))
                .when(Condition::Custom {
                    name: "compute-sshr".to_string(),
                    check: |bindings| {
                        if let (Some(a), Some(b)) = (
                            bindings.get_constant(&VarId::new("a")),
                            bindings.get_constant(&VarId::new("b")),
                        ) {
                            if b >= 0 && b < 64 {
                                unsafe {
                                    let bp = bindings as *const Bindings as *mut Bindings;
                                    (*bp)
                                        .constants
                                        .insert(VarId::new("result"), a.wrapping_shr(b as u32));
                                }
                                true
                            } else {
                                false
                            }
                        } else {
                            false
                        }
                    },
                })
                .build(),
        );

        // const-fold-ineg: ineg(a) => -a
        rules.push(
            Rewrite::new("const-fold-ineg")
                .match_pattern(Pattern::op(Opcode::Ineg, vec![Pattern::any_constant("a")]))
                .produce(Pattern::any_constant("result"))
                .when(Condition::Custom {
                    name: "compute-ineg".to_string(),
                    check: |bindings| {
                        if let Some(a) = bindings.get_constant(&VarId::new("a")) {
                            unsafe {
                                let bp = bindings as *const Bindings as *mut Bindings;
                                (*bp)
                                    .constants
                                    .insert(VarId::new("result"), a.wrapping_neg());
                            }
                            true
                        } else {
                            false
                        }
                    },
                })
                .build(),
        );

        // const-fold-bnot: bnot(a) => !a
        rules.push(
            Rewrite::new("const-fold-bnot")
                .match_pattern(Pattern::op(Opcode::Bnot, vec![Pattern::any_constant("a")]))
                .produce(Pattern::any_constant("result"))
                .when(Condition::Custom {
                    name: "compute-bnot".to_string(),
                    check: |bindings| {
                        if let Some(a) = bindings.get_constant(&VarId::new("a")) {
                            unsafe {
                                let bp = bindings as *const Bindings as *mut Bindings;
                                (*bp).constants.insert(VarId::new("result"), !a);
                            }
                            true
                        } else {
                            false
                        }
                    },
                })
                .build(),
        );

        // const-fold-iabs: iabs(a) => |a|
        rules.push(
            Rewrite::new("const-fold-iabs")
                .match_pattern(Pattern::op(Opcode::Iabs, vec![Pattern::any_constant("a")]))
                .produce(Pattern::any_constant("result"))
                .when(Condition::Custom {
                    name: "compute-iabs".to_string(),
                    check: |bindings| {
                        if let Some(a) = bindings.get_constant(&VarId::new("a")) {
                            unsafe {
                                let bp = bindings as *const Bindings as *mut Bindings;
                                (*bp)
                                    .constants
                                    .insert(VarId::new("result"), a.wrapping_abs());
                            }
                            true
                        } else {
                            false
                        }
                    },
                })
                .build(),
        );

        // const-fold-clz: clz(a) => leading zeros of a
        rules.push(
            Rewrite::new("const-fold-clz")
                .match_pattern(Pattern::op(Opcode::Clz, vec![Pattern::any_constant("a")]))
                .produce(Pattern::any_constant("result"))
                .when(Condition::Custom {
                    name: "compute-clz".to_string(),
                    check: |bindings| {
                        if let Some(a) = bindings.get_constant(&VarId::new("a")) {
                            unsafe {
                                let bp = bindings as *const Bindings as *mut Bindings;
                                (*bp).constants.insert(
                                    VarId::new("result"),
                                    (a as u64).leading_zeros() as i64,
                                );
                            }
                            true
                        } else {
                            false
                        }
                    },
                })
                .build(),
        );

        // const-fold-ctz: ctz(a) => trailing zeros of a
        rules.push(
            Rewrite::new("const-fold-ctz")
                .match_pattern(Pattern::op(Opcode::Ctz, vec![Pattern::any_constant("a")]))
                .produce(Pattern::any_constant("result"))
                .when(Condition::Custom {
                    name: "compute-ctz".to_string(),
                    check: |bindings| {
                        if let Some(a) = bindings.get_constant(&VarId::new("a")) {
                            unsafe {
                                let bp = bindings as *const Bindings as *mut Bindings;
                                (*bp).constants.insert(
                                    VarId::new("result"),
                                    (a as u64).trailing_zeros() as i64,
                                );
                            }
                            true
                        } else {
                            false
                        }
                    },
                })
                .build(),
        );

        // const-fold-rotl: rotl(a, b) => a.rotate_left(b)
        rules.push(
            Rewrite::new("const-fold-rotl")
                .match_pattern(Pattern::op(
                    Opcode::Rotl,
                    vec![Pattern::any_constant("a"), Pattern::any_constant("b")],
                ))
                .produce(Pattern::any_constant("result"))
                .when(Condition::Custom {
                    name: "compute-rotl".to_string(),
                    check: |bindings| {
                        if let (Some(a), Some(b)) = (
                            bindings.get_constant(&VarId::new("a")),
                            bindings.get_constant(&VarId::new("b")),
                        ) {
                            unsafe {
                                let bp = bindings as *const Bindings as *mut Bindings;
                                (*bp).constants.insert(
                                    VarId::new("result"),
                                    (a as u64).rotate_left((b & 63) as u32) as i64,
                                );
                            }
                            true
                        } else {
                            false
                        }
                    },
                })
                .build(),
        );

        // const-fold-rotr: rotr(a, b) => a.rotate_right(b)
        rules.push(
            Rewrite::new("const-fold-rotr")
                .match_pattern(Pattern::op(
                    Opcode::Rotr,
                    vec![Pattern::any_constant("a"), Pattern::any_constant("b")],
                ))
                .produce(Pattern::any_constant("result"))
                .when(Condition::Custom {
                    name: "compute-rotr".to_string(),
                    check: |bindings| {
                        if let (Some(a), Some(b)) = (
                            bindings.get_constant(&VarId::new("a")),
                            bindings.get_constant(&VarId::new("b")),
                        ) {
                            unsafe {
                                let bp = bindings as *const Bindings as *mut Bindings;
                                (*bp).constants.insert(
                                    VarId::new("result"),
                                    (a as u64).rotate_right((b & 63) as u32) as i64,
                                );
                            }
                            true
                        } else {
                            false
                        }
                    },
                })
                .build(),
        );

        // const-fold-eq: eq(a, b) => (a == b) as i64
        rules.push(
            Rewrite::new("const-fold-eq")
                .match_pattern(Pattern::op(
                    Opcode::Eq,
                    vec![Pattern::any_constant("a"), Pattern::any_constant("b")],
                ))
                .produce(Pattern::any_constant("result"))
                .when(Condition::Custom {
                    name: "compute-eq".to_string(),
                    check: |bindings| {
                        if let (Some(a), Some(b)) = (
                            bindings.get_constant(&VarId::new("a")),
                            bindings.get_constant(&VarId::new("b")),
                        ) {
                            unsafe {
                                let bp = bindings as *const Bindings as *mut Bindings;
                                (*bp)
                                    .constants
                                    .insert(VarId::new("result"), if a == b { 1 } else { 0 });
                            }
                            true
                        } else {
                            false
                        }
                    },
                })
                .build(),
        );

        // const-fold-ne: ne(a, b) => (a != b) as i64
        rules.push(
            Rewrite::new("const-fold-ne")
                .match_pattern(Pattern::op(
                    Opcode::Ne,
                    vec![Pattern::any_constant("a"), Pattern::any_constant("b")],
                ))
                .produce(Pattern::any_constant("result"))
                .when(Condition::Custom {
                    name: "compute-ne".to_string(),
                    check: |bindings| {
                        if let (Some(a), Some(b)) = (
                            bindings.get_constant(&VarId::new("a")),
                            bindings.get_constant(&VarId::new("b")),
                        ) {
                            unsafe {
                                let bp = bindings as *const Bindings as *mut Bindings;
                                (*bp)
                                    .constants
                                    .insert(VarId::new("result"), if a != b { 1 } else { 0 });
                            }
                            true
                        } else {
                            false
                        }
                    },
                })
                .build(),
        );

        // const-fold-slt: slt(a, b) => (a < b) as i64
        rules.push(
            Rewrite::new("const-fold-slt")
                .match_pattern(Pattern::op(
                    Opcode::Slt,
                    vec![Pattern::any_constant("a"), Pattern::any_constant("b")],
                ))
                .produce(Pattern::any_constant("result"))
                .when(Condition::Custom {
                    name: "compute-slt".to_string(),
                    check: |bindings| {
                        if let (Some(a), Some(b)) = (
                            bindings.get_constant(&VarId::new("a")),
                            bindings.get_constant(&VarId::new("b")),
                        ) {
                            unsafe {
                                let bp = bindings as *const Bindings as *mut Bindings;
                                (*bp)
                                    .constants
                                    .insert(VarId::new("result"), if a < b { 1 } else { 0 });
                            }
                            true
                        } else {
                            false
                        }
                    },
                })
                .build(),
        );

        // const-fold-sle: sle(a, b) => (a <= b) as i64
        rules.push(
            Rewrite::new("const-fold-sle")
                .match_pattern(Pattern::op(
                    Opcode::Sle,
                    vec![Pattern::any_constant("a"), Pattern::any_constant("b")],
                ))
                .produce(Pattern::any_constant("result"))
                .when(Condition::Custom {
                    name: "compute-sle".to_string(),
                    check: |bindings| {
                        if let (Some(a), Some(b)) = (
                            bindings.get_constant(&VarId::new("a")),
                            bindings.get_constant(&VarId::new("b")),
                        ) {
                            unsafe {
                                let bp = bindings as *const Bindings as *mut Bindings;
                                (*bp)
                                    .constants
                                    .insert(VarId::new("result"), if a <= b { 1 } else { 0 });
                            }
                            true
                        } else {
                            false
                        }
                    },
                })
                .build(),
        );

        // const-fold-select: select(c, a, b) where c is constant
        rules.push(
            Rewrite::new("const-fold-select")
                .match_pattern(Pattern::op(
                    Opcode::Select,
                    vec![
                        Pattern::any_constant("c"),
                        Pattern::any_constant("a"),
                        Pattern::any_constant("b"),
                    ],
                ))
                .produce(Pattern::any_constant("result"))
                .when(Condition::Custom {
                    name: "compute-select".to_string(),
                    check: |bindings| {
                        if let (Some(c), Some(a), Some(b)) = (
                            bindings.get_constant(&VarId::new("c")),
                            bindings.get_constant(&VarId::new("a")),
                            bindings.get_constant(&VarId::new("b")),
                        ) {
                            unsafe {
                                let bp = bindings as *const Bindings as *mut Bindings;
                                (*bp)
                                    .constants
                                    .insert(VarId::new("result"), if c != 0 { a } else { b });
                            }
                            true
                        } else {
                            false
                        }
                    },
                })
                .build(),
        );

        // ===== ISLE-derived Icmp Simplification Rules =====

        // eq(x, 0) where x is boolean (range [0,1]) => bnot-like: xor(x, 1)
        // Not exactly ISLE but useful: slt(x, 1) => eq(x, 0) when x in [0,1]

        // ult(x, 0) => 0  (nothing is unsigned-less-than 0)
        rules.push(
            Rewrite::new("ult-zero-false")
                .match_pattern(Pattern::op(
                    Opcode::Ult,
                    vec![Pattern::var("x"), Pattern::constant(0)],
                ))
                .produce(Pattern::constant(0))
                .build(),
        );

        // uge(x, 0) => 1  (everything is unsigned-gte 0)
        rules.push(
            Rewrite::new("uge-zero-true")
                .match_pattern(Pattern::op(
                    Opcode::Uge,
                    vec![Pattern::var("x"), Pattern::constant(0)],
                ))
                .produce(Pattern::constant(1))
                .build(),
        );

        // ule(0, x) => 1  (0 is unsigned-le everything)
        rules.push(
            Rewrite::new("ule-zero-left-true")
                .match_pattern(Pattern::op(
                    Opcode::Ule,
                    vec![Pattern::constant(0), Pattern::var("x")],
                ))
                .produce(Pattern::constant(1))
                .build(),
        );

        // ugt(0, x) => 0  (0 is never unsigned-gt anything)
        rules.push(
            Rewrite::new("ugt-zero-left-false")
                .match_pattern(Pattern::op(
                    Opcode::Ugt,
                    vec![Pattern::constant(0), Pattern::var("x")],
                ))
                .produce(Pattern::constant(0))
                .build(),
        );

        // eq(x, x) already handled above

        // ne(eq(x, y), 0) => eq(x, y)  (boolean ne 0 is identity)
        rules.push(
            Rewrite::new("ne-eq-zero")
                .match_pattern(Pattern::op(
                    Opcode::Ne,
                    vec![
                        Pattern::op(Opcode::Eq, vec![Pattern::var("x"), Pattern::var("y")]),
                        Pattern::constant(0),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Eq,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .build(),
        );

        // eq(eq(x, y), 0) => ne(x, y)  (boolean eq 0 is negation)
        rules.push(
            Rewrite::new("eq-eq-zero-to-ne")
                .match_pattern(Pattern::op(
                    Opcode::Eq,
                    vec![
                        Pattern::op(Opcode::Eq, vec![Pattern::var("x"), Pattern::var("y")]),
                        Pattern::constant(0),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Ne,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .build(),
        );

        // eq(ne(x, y), 0) => eq(x, y)
        rules.push(
            Rewrite::new("eq-ne-zero-to-eq")
                .match_pattern(Pattern::op(
                    Opcode::Eq,
                    vec![
                        Pattern::op(Opcode::Ne, vec![Pattern::var("x"), Pattern::var("y")]),
                        Pattern::constant(0),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Eq,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .build(),
        );

        // ne(ne(x, y), 0) => ne(x, y)
        rules.push(
            Rewrite::new("ne-ne-zero")
                .match_pattern(Pattern::op(
                    Opcode::Ne,
                    vec![
                        Pattern::op(Opcode::Ne, vec![Pattern::var("x"), Pattern::var("y")]),
                        Pattern::constant(0),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Ne,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .build(),
        );

        // Comparison with bnot: eq(bnot(x), bnot(y)) => eq(x, y)
        rules.push(
            Rewrite::new("eq-bnot-bnot")
                .match_pattern(Pattern::op(
                    Opcode::Eq,
                    vec![
                        Pattern::op(Opcode::Bnot, vec![Pattern::var("x")]),
                        Pattern::op(Opcode::Bnot, vec![Pattern::var("y")]),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Eq,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .build(),
        );

        // ne(bnot(x), bnot(y)) => ne(x, y)
        rules.push(
            Rewrite::new("ne-bnot-bnot")
                .match_pattern(Pattern::op(
                    Opcode::Ne,
                    vec![
                        Pattern::op(Opcode::Bnot, vec![Pattern::var("x")]),
                        Pattern::op(Opcode::Bnot, vec![Pattern::var("y")]),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Ne,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .build(),
        );

        // Comparison with ineg: slt(ineg(x), ineg(y)) => sgt(y, x)  (ISLE: flip signed cmp under negation)
        rules.push(
            Rewrite::new("slt-ineg-ineg")
                .match_pattern(Pattern::op(
                    Opcode::Slt,
                    vec![
                        Pattern::op(Opcode::Ineg, vec![Pattern::var("x")]),
                        Pattern::op(Opcode::Ineg, vec![Pattern::var("y")]),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Slt,
                    vec![Pattern::var("y"), Pattern::var("x")],
                ))
                .build(),
        );

        // sle(ineg(x), ineg(y)) => sle(y, x)
        rules.push(
            Rewrite::new("sle-ineg-ineg")
                .match_pattern(Pattern::op(
                    Opcode::Sle,
                    vec![
                        Pattern::op(Opcode::Ineg, vec![Pattern::var("x")]),
                        Pattern::op(Opcode::Ineg, vec![Pattern::var("y")]),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Sle,
                    vec![Pattern::var("y"), Pattern::var("x")],
                ))
                .build(),
        );

        // Offset cancellation in comparisons: eq(add(x, c), add(y, c)) => eq(x, y)
        rules.push(
            Rewrite::new("eq-add-cancel")
                .match_pattern(Pattern::op(
                    Opcode::Eq,
                    vec![
                        Pattern::op(Opcode::Add, vec![Pattern::var("x"), Pattern::var("c")]),
                        Pattern::op(Opcode::Add, vec![Pattern::var("y"), Pattern::var("c")]),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Eq,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .build(),
        );

        // ne(add(x, c), add(y, c)) => ne(x, y)
        rules.push(
            Rewrite::new("ne-add-cancel")
                .match_pattern(Pattern::op(
                    Opcode::Ne,
                    vec![
                        Pattern::op(Opcode::Add, vec![Pattern::var("x"), Pattern::var("c")]),
                        Pattern::op(Opcode::Add, vec![Pattern::var("y"), Pattern::var("c")]),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Ne,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .build(),
        );

        // slt(add(x, c), add(y, c)) => slt(x, y)  (signed: offset doesn't change order if no overflow)
        rules.push(
            Rewrite::new("slt-add-cancel")
                .match_pattern(Pattern::op(
                    Opcode::Slt,
                    vec![
                        Pattern::op(Opcode::Add, vec![Pattern::var("x"), Pattern::var("c")]),
                        Pattern::op(Opcode::Add, vec![Pattern::var("y"), Pattern::var("c")]),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Slt,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .build(),
        );

        // ===== ISLE-derived Extend/Reduce Rules =====

        // uextend(uextend(x)) => uextend(x)  (chain collapse)
        rules.push(
            Rewrite::new("uextend-uextend")
                .match_pattern(Pattern::op(
                    Opcode::Uextend,
                    vec![Pattern::op(Opcode::Uextend, vec![Pattern::var("x")])],
                ))
                .produce(Pattern::op(Opcode::Uextend, vec![Pattern::var("x")]))
                .build(),
        );

        // sextend(sextend(x)) => sextend(x)  (chain collapse)
        rules.push(
            Rewrite::new("sextend-sextend")
                .match_pattern(Pattern::op(
                    Opcode::Sextend,
                    vec![Pattern::op(Opcode::Sextend, vec![Pattern::var("x")])],
                ))
                .produce(Pattern::op(Opcode::Sextend, vec![Pattern::var("x")]))
                .build(),
        );

        // ireduce(ireduce(x)) => ireduce(x)  (chain collapse)
        rules.push(
            Rewrite::new("ireduce-ireduce")
                .match_pattern(Pattern::op(
                    Opcode::Ireduce,
                    vec![Pattern::op(Opcode::Ireduce, vec![Pattern::var("x")])],
                ))
                .produce(Pattern::op(Opcode::Ireduce, vec![Pattern::var("x")]))
                .build(),
        );

        // ireduce(uextend(x)) => x  (narrow then widen cancels)
        rules.push(
            Rewrite::new("ireduce-uextend")
                .match_pattern(Pattern::op(
                    Opcode::Ireduce,
                    vec![Pattern::op(Opcode::Uextend, vec![Pattern::var("x")])],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        // ireduce(sextend(x)) => x
        rules.push(
            Rewrite::new("ireduce-sextend")
                .match_pattern(Pattern::op(
                    Opcode::Ireduce,
                    vec![Pattern::op(Opcode::Sextend, vec![Pattern::var("x")])],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

        // and(uextend(x), uextend(y)) => uextend(and(x, y))  (push bitwise under extend)
        rules.push(
            Rewrite::new("and-uextend")
                .match_pattern(Pattern::op(
                    Opcode::And,
                    vec![
                        Pattern::op(Opcode::Uextend, vec![Pattern::var("x")]),
                        Pattern::op(Opcode::Uextend, vec![Pattern::var("y")]),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Uextend,
                    vec![Pattern::op(
                        Opcode::And,
                        vec![Pattern::var("x"), Pattern::var("y")],
                    )],
                ))
                .build(),
        );

        // or(uextend(x), uextend(y)) => uextend(or(x, y))
        rules.push(
            Rewrite::new("or-uextend")
                .match_pattern(Pattern::op(
                    Opcode::Or,
                    vec![
                        Pattern::op(Opcode::Uextend, vec![Pattern::var("x")]),
                        Pattern::op(Opcode::Uextend, vec![Pattern::var("y")]),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Uextend,
                    vec![Pattern::op(
                        Opcode::Or,
                        vec![Pattern::var("x"), Pattern::var("y")],
                    )],
                ))
                .build(),
        );

        // xor(uextend(x), uextend(y)) => uextend(xor(x, y))
        rules.push(
            Rewrite::new("xor-uextend")
                .match_pattern(Pattern::op(
                    Opcode::Xor,
                    vec![
                        Pattern::op(Opcode::Uextend, vec![Pattern::var("x")]),
                        Pattern::op(Opcode::Uextend, vec![Pattern::var("y")]),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Uextend,
                    vec![Pattern::op(
                        Opcode::Xor,
                        vec![Pattern::var("x"), Pattern::var("y")],
                    )],
                ))
                .build(),
        );

        // add(uextend(x), uextend(y)) => uextend(add(x, y))  (when no overflow)
        rules.push(
            Rewrite::new("add-uextend")
                .match_pattern(Pattern::op(
                    Opcode::Add,
                    vec![
                        Pattern::op(Opcode::Uextend, vec![Pattern::var("x")]),
                        Pattern::op(Opcode::Uextend, vec![Pattern::var("y")]),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Uextend,
                    vec![Pattern::op(
                        Opcode::Add,
                        vec![Pattern::var("x"), Pattern::var("y")],
                    )],
                ))
                .build(),
        );

        // bnot(uextend(x)) => uextend(bnot(x))
        rules.push(
            Rewrite::new("bnot-uextend")
                .match_pattern(Pattern::op(
                    Opcode::Bnot,
                    vec![Pattern::op(Opcode::Uextend, vec![Pattern::var("x")])],
                ))
                .produce(Pattern::op(
                    Opcode::Uextend,
                    vec![Pattern::op(Opcode::Bnot, vec![Pattern::var("x")])],
                ))
                .build(),
        );

        // ineg(sextend(x)) => sextend(ineg(x))
        rules.push(
            Rewrite::new("ineg-sextend")
                .match_pattern(Pattern::op(
                    Opcode::Ineg,
                    vec![Pattern::op(Opcode::Sextend, vec![Pattern::var("x")])],
                ))
                .produce(Pattern::op(
                    Opcode::Sextend,
                    vec![Pattern::op(Opcode::Ineg, vec![Pattern::var("x")])],
                ))
                .build(),
        );

        // iabs(sextend(x)) => uextend(iabs(x))  (abs of sign-extended = zero-extend of abs)
        rules.push(
            Rewrite::new("iabs-sextend")
                .match_pattern(Pattern::op(
                    Opcode::Iabs,
                    vec![Pattern::op(Opcode::Sextend, vec![Pattern::var("x")])],
                ))
                .produce(Pattern::op(
                    Opcode::Uextend,
                    vec![Pattern::op(Opcode::Iabs, vec![Pattern::var("x")])],
                ))
                .build(),
        );

        // eq(uextend(x), uextend(y)) => eq(x, y)  (comparison through extend)
        rules.push(
            Rewrite::new("eq-uextend-uextend")
                .match_pattern(Pattern::op(
                    Opcode::Eq,
                    vec![
                        Pattern::op(Opcode::Uextend, vec![Pattern::var("x")]),
                        Pattern::op(Opcode::Uextend, vec![Pattern::var("y")]),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Eq,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .build(),
        );

        // ne(uextend(x), uextend(y)) => ne(x, y)
        rules.push(
            Rewrite::new("ne-uextend-uextend")
                .match_pattern(Pattern::op(
                    Opcode::Ne,
                    vec![
                        Pattern::op(Opcode::Uextend, vec![Pattern::var("x")]),
                        Pattern::op(Opcode::Uextend, vec![Pattern::var("y")]),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Ne,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .build(),
        );

        // eq(sextend(x), sextend(y)) => eq(x, y)
        rules.push(
            Rewrite::new("eq-sextend-sextend")
                .match_pattern(Pattern::op(
                    Opcode::Eq,
                    vec![
                        Pattern::op(Opcode::Sextend, vec![Pattern::var("x")]),
                        Pattern::op(Opcode::Sextend, vec![Pattern::var("y")]),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Eq,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .build(),
        );

        // ne(sextend(x), sextend(y)) => ne(x, y)
        rules.push(
            Rewrite::new("ne-sextend-sextend")
                .match_pattern(Pattern::op(
                    Opcode::Ne,
                    vec![
                        Pattern::op(Opcode::Sextend, vec![Pattern::var("x")]),
                        Pattern::op(Opcode::Sextend, vec![Pattern::var("y")]),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Ne,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .build(),
        );

        // slt(sextend(x), sextend(y)) => slt(x, y)
        rules.push(
            Rewrite::new("slt-sextend-sextend")
                .match_pattern(Pattern::op(
                    Opcode::Slt,
                    vec![
                        Pattern::op(Opcode::Sextend, vec![Pattern::var("x")]),
                        Pattern::op(Opcode::Sextend, vec![Pattern::var("y")]),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Slt,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .build(),
        );

        // ult(uextend(x), uextend(y)) => ult(x, y)
        rules.push(
            Rewrite::new("ult-uextend-uextend")
                .match_pattern(Pattern::op(
                    Opcode::Ult,
                    vec![
                        Pattern::op(Opcode::Uextend, vec![Pattern::var("x")]),
                        Pattern::op(Opcode::Uextend, vec![Pattern::var("y")]),
                    ],
                ))
                .produce(Pattern::op(
                    Opcode::Ult,
                    vec![Pattern::var("x"), Pattern::var("y")],
                ))
                .build(),
        );

        // irem(x, n) => x  when range proves x ∈ [0, n-1]
        rules.push(
            Rewrite::new("irem-identity-range")
                .match_pattern(Pattern::op(
                    Opcode::Irem,
                    vec![Pattern::var("x"), Pattern::any_constant("n")],
                ))
                .produce(Pattern::var("x"))
                .when(Condition::Custom {
                    name: "x-range-lt-n-nonneg".to_string(),
                    check: |bindings| {
                        let n = match bindings.get_constant(&VarId::new("n")) {
                            Some(n) if n > 0 => n,
                            _ => return false,
                        };
                        bindings
                            .get_value_range(&VarId::new("x"))
                            .map(|r| r.min >= 0 && r.max < n)
                            .unwrap_or(false)
                    },
                })
                .build(),
        );

        // Sign-bit nonzero pattern:
        // band(sshr(bor(x, ineg(x)), 31), 1) => 1 when x is nonzero
        //
        // For any nonzero 32-bit integer x, bor(x, -x) always has the MSB set,
        // so sshr by 31 gives -1 (all ones), and band(-1, 1) = 1.
        rules.push(
            Rewrite::new("sign-bit-nonzero")
                .match_pattern(Pattern::op(
                    Opcode::And,
                    vec![
                        Pattern::op(
                            Opcode::Sshr,
                            vec![
                                Pattern::op(
                                    Opcode::Or,
                                    vec![
                                        Pattern::var("x"),
                                        Pattern::op(Opcode::Ineg, vec![Pattern::var("x")]),
                                    ],
                                ),
                                Pattern::constant(31),
                            ],
                        ),
                        Pattern::constant(1),
                    ],
                ))
                .produce(Pattern::constant(1))
                .when(Condition::nonzero("x"))
                .build(),
        );

        Self { rules }
    }

    /// Add a custom rule
    pub fn add_rule(&mut self, rule: Rewrite) {
        self.rules.push(rule);
    }

    /// Get all rules
    pub fn iter(&self) -> impl Iterator<Item = &Rewrite> {
        self.rules.iter()
    }
}
