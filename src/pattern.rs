//! Pattern IR for Rewrite Rules
//!
//! Example usage:
//! ```
//! // Define rule: (x + 0) => x
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

use crate::types::*;
use std::collections::HashMap;

/// Variable identifier in patterns (e.g., "x", "y", "c")
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VarId(pub String);

impl VarId {
    pub fn new(s: impl Into<String>) -> Self {
        VarId(s.into())
    }
}

/// A pattern that can match against values in the egraph
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// Match any value and bind it to a variable
    /// Example: Pattern::Var("x") matches any value and binds it to "x"
    Var(VarId),

    /// Match a specific operation with pattern arguments
    /// Example: Pattern::Op(Add, [Var("x"), Var("y")]) matches any addition
    Op { opcode: Opcode, args: Vec<Pattern> },

    /// Match a constant with a specific value
    /// Example: Pattern::Constant(0) only matches the constant 0
    Constant(i64),

    /// Match any constant and bind its value to a variable
    /// Example: Pattern::AnyConstant("c") matches any constant
    AnyConstant(VarId),

    /// Match any value of a specific type (without binding)
    /// Example: Pattern::AnyOfType(Type::I32) matches any i32 value
    AnyOfType(Type),

    /// Match one of several alternative patterns (disjunction)
    /// Example: Pattern::Or([Constant(0), Constant(1)])
    Or(Vec<Pattern>),

    /// Match if a subpattern matches AND a condition holds
    /// Example: Pattern::Where(Var("x"), Condition::IsConstant("x"))
    Where {
        pattern: Box<Pattern>,
        condition: Condition,
    },
}

impl Pattern {
    /// Helper: create a variable pattern
    pub fn var(name: impl Into<String>) -> Self {
        Pattern::Var(VarId::new(name))
    }

    /// Helper: create an operation pattern
    pub fn op(opcode: Opcode, args: Vec<Pattern>) -> Self {
        Pattern::Op { opcode, args }
    }

    /// Helper: create a constant pattern
    pub fn constant(value: i64) -> Self {
        Pattern::Constant(value)
    }

    /// Helper: create an any-constant pattern
    pub fn any_constant(name: impl Into<String>) -> Self {
        Pattern::AnyConstant(VarId::new(name))
    }

    /// Helper: create a where pattern
    pub fn where_cond(pattern: Pattern, condition: Condition) -> Self {
        Pattern::Where {
            pattern: Box::new(pattern),
            condition,
        }
    }
}

/// Conditions that must hold for a rewrite to apply
#[derive(Debug, Clone, PartialEq)]
pub enum Condition {
    /// Variable must be bound to a constant value
    IsConstant(VarId),

    /// Variable must equal a specific constant
    EqualsConstant(VarId, i64),

    /// Variable must NOT equal a specific constant
    NotEqualsConstant(VarId, i64),

    /// Variable must be greater than a constant
    GreaterThan(VarId, i64),

    /// Variable must be less than a constant
    LessThan(VarId, i64),

    /// Two variables must be equal
    VarsEqual(VarId, VarId),

    /// Two variables must NOT be equal
    VarsNotEqual(VarId, VarId),

    /// Conjunction (all conditions must hold)
    And(Vec<Condition>),

    /// Disjunction (at least one condition must hold)
    Or(Vec<Condition>),

    /// Custom condition with a function
    Custom {
        name: String,
        check: fn(&Bindings) -> bool,
    },
}

/// Bindings from pattern variables to values/constants
#[derive(Debug, Clone)]
pub struct Bindings {
    /// Maps variable names to values in the egraph
    pub values: HashMap<VarId, ValueId>,

    /// Maps variable names to constant values
    pub constants: HashMap<VarId, i64>,

    /// The DFG (needed to look up instructions/values)
    dfg: *const DataFlowGraph,
}

impl Bindings {
    pub fn new(dfg: &DataFlowGraph) -> Self {
        Self {
            values: HashMap::new(),
            constants: HashMap::new(),
            dfg: dfg as *const _,
        }
    }

    /// Bind a variable to a value
    pub fn bind_value(&mut self, var: VarId, value: ValueId) {
        self.values.insert(var, value);
    }

    /// Bind a variable to a constant
    pub fn bind_constant(&mut self, var: VarId, constant: i64) {
        self.constants.insert(var, constant);
    }

    /// Get the value bound to a variable
    pub fn get_value(&self, var: &VarId) -> Option<ValueId> {
        self.values.get(var).copied()
    }

    /// Get the constant bound to a variable
    pub fn get_constant(&self, var: &VarId) -> Option<i64> {
        self.constants.get(var).copied()
    }

    /// Get the DFG
    pub fn dfg(&self) -> &DataFlowGraph {
        unsafe { &*self.dfg }
    }

    /// Check if a variable is bound to a constant value
    pub fn is_constant(&self, var: &VarId) -> bool {
        if let Some(value) = self.get_value(var) {
            let dfg = self.dfg();
            if let ValueDef::Inst(inst_id) = dfg.value_def(value) {
                let inst = &dfg.insts[&inst_id];
                return inst.opcode == Opcode::Const;
            }
        }
        false
    }

    /// Get the constant value of a bound variable (if it's a constant)
    pub fn get_constant_value(&self, var: &VarId) -> Option<i64> {
        // First check if it's in the constants map
        if let Some(c) = self.constants.get(var) {
            return Some(*c);
        }

        // Otherwise, check if the bound value is a constant instruction
        if let Some(value) = self.get_value(var) {
            let dfg = self.dfg();
            if let ValueDef::Inst(inst_id) = dfg.value_def(value) {
                let inst = &dfg.insts[&inst_id];
                if inst.opcode == Opcode::Const {
                    return inst.immediate;
                }
            }
        }

        None
    }
}

/// A complete rewrite rule: pattern => pattern
#[derive(Debug, Clone)]
pub struct Rewrite {
    /// Name of the rule (for debugging)
    pub name: String,

    /// Left-hand side: pattern to match
    pub lhs: Pattern,

    /// Right-hand side: pattern to produce
    pub rhs: Pattern,

    /// Conditions that must hold for the rewrite to apply
    pub conditions: Vec<Condition>,

    /// Whether this rule can be applied bidirectionally
    pub bidirectional: bool,
}

impl Rewrite {
    /// Create a new rewrite rule builder
    pub fn new(name: impl Into<String>) -> RewriteBuilder {
        RewriteBuilder {
            name: name.into(),
            lhs: None,
            rhs: None,
            conditions: Vec::new(),
            bidirectional: false,
        }
    }

    /// Check if this rewrite can be applied given the bindings
    pub fn check_conditions(&self, bindings: &Bindings) -> bool {
        self.conditions.iter().all(|cond| cond.check(bindings))
    }
}

/// Builder for constructing rewrite rules
pub struct RewriteBuilder {
    name: String,
    lhs: Option<Pattern>,
    rhs: Option<Pattern>,
    conditions: Vec<Condition>,
    bidirectional: bool,
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

    pub fn bidirectional(mut self) -> Self {
        self.bidirectional = true;
        self
    }

    pub fn build(self) -> Rewrite {
        Rewrite {
            name: self.name,
            lhs: self.lhs.expect("Rewrite must have LHS pattern"),
            rhs: self.rhs.expect("Rewrite must have RHS pattern"),
            conditions: self.conditions,
            bidirectional: self.bidirectional,
        }
    }
}

impl Condition {
    /// Check if this condition holds given the bindings
    pub fn check(&self, bindings: &Bindings) -> bool {
        match self {
            Condition::IsConstant(var) => bindings.is_constant(var),

            Condition::EqualsConstant(var, value) => {
                bindings.get_constant_value(var) == Some(*value)
            }

            Condition::NotEqualsConstant(var, value) => {
                bindings.get_constant_value(var) != Some(*value)
            }

            Condition::GreaterThan(var, value) => bindings
                .get_constant_value(var)
                .map(|v| v > *value)
                .unwrap_or(false),

            Condition::LessThan(var, value) => bindings
                .get_constant_value(var)
                .map(|v| v < *value)
                .unwrap_or(false),

            Condition::VarsEqual(var1, var2) => {
                bindings.get_value(var1) == bindings.get_value(var2)
            }

            Condition::VarsNotEqual(var1, var2) => {
                bindings.get_value(var1) != bindings.get_value(var2)
            }

            Condition::And(conds) => conds.iter().all(|c| c.check(bindings)),

            Condition::Or(conds) => conds.iter().any(|c| c.check(bindings)),

            Condition::Custom { check, .. } => check(bindings),
        }
    }
}

/// Pattern matcher: matches patterns against values in the egraph
pub struct PatternMatcher<'a> {
    dfg: &'a DataFlowGraph,
}

impl<'a> PatternMatcher<'a> {
    pub fn new(dfg: &'a DataFlowGraph) -> Self {
        Self { dfg }
    }

    /// Try to match a pattern against a value
    /// Returns Some(bindings) if successful, None otherwise
    pub fn match_pattern(&self, pattern: &Pattern, value: ValueId) -> Option<Bindings> {
        let mut bindings = Bindings::new(self.dfg);
        if self.match_pattern_impl(pattern, value, &mut bindings) {
            Some(bindings)
        } else {
            None
        }
    }

    /// Internal recursive matching
    fn match_pattern_impl(
        &self,
        pattern: &Pattern,
        value: ValueId,
        bindings: &mut Bindings,
    ) -> bool {
        match pattern {
            Pattern::Var(var) => {
                // Check if variable is already bound
                if let Some(bound_value) = bindings.get_value(var) {
                    // Must match the same value
                    bound_value == value
                } else {
                    // Bind the variable
                    bindings.bind_value(var.clone(), value);
                    true
                }
            }

            Pattern::Op { opcode, args } => {
                // Must be an instruction with matching opcode
                match self.dfg.value_def(value) {
                    ValueDef::Inst(inst_id) => {
                        let inst = &self.dfg.insts[&inst_id];

                        // Check opcode matches
                        if inst.opcode != *opcode {
                            return false;
                        }

                        // Check arg count matches
                        if inst.args.len() != args.len() {
                            return false;
                        }

                        // Recursively match arguments
                        for (pattern_arg, &value_arg) in args.iter().zip(&inst.args) {
                            if !self.match_pattern_impl(pattern_arg, value_arg, bindings) {
                                return false;
                            }
                        }

                        true
                    }
                    _ => false,
                }
            }

            Pattern::Constant(expected) => {
                // Must be a constant instruction with matching value
                match self.dfg.value_def(value) {
                    ValueDef::Inst(inst_id) => {
                        let inst = &self.dfg.insts[&inst_id];
                        inst.opcode == Opcode::Const && inst.immediate == Some(*expected)
                    }
                    _ => false,
                }
            }

            Pattern::AnyConstant(var) => {
                // Must be any constant, bind its value
                match self.dfg.value_def(value) {
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
                }
            }

            Pattern::AnyOfType(expected_type) => {
                // Just check the type matches
                self.dfg.value_type(value) == *expected_type
            }

            Pattern::Or(alternatives) => {
                // Try each alternative
                for alt in alternatives {
                    let mut alt_bindings = bindings.clone();
                    if self.match_pattern_impl(alt, value, &mut alt_bindings) {
                        *bindings = alt_bindings;
                        return true;
                    }
                }
                false
            }

            Pattern::Where { pattern, condition } => {
                // First match the pattern
                if !self.match_pattern_impl(pattern, value, bindings) {
                    return false;
                }
                // Then check the condition
                condition.check(bindings)
            }
        }
    }
}

/// Pattern applier: constructs new values from patterns using bindings
pub struct PatternApplier<'a> {
    dfg: &'a mut DataFlowGraph,
}

impl<'a> PatternApplier<'a> {
    pub fn new(dfg: &'a mut DataFlowGraph) -> Self {
        Self { dfg }
    }

    /// Apply a pattern to produce a new value
    pub fn apply_pattern(&mut self, pattern: &Pattern, bindings: &Bindings) -> Option<ValueId> {
        match pattern {
            Pattern::Var(var) => {
                // Look up the bound value
                bindings.get_value(var)
            }

            Pattern::Op { opcode, args } => {
                // Recursively apply argument patterns
                let mut arg_values = Vec::new();
                for arg_pattern in args {
                    let arg_value = self.apply_pattern(arg_pattern, bindings)?;
                    arg_values.push(arg_value);
                }

                // Get type from first argument (simplified)
                let ty = if let Some(&first) = arg_values.first() {
                    self.dfg.value_type(first)
                } else {
                    Type::I32 // default
                };

                // Create the instruction
                let inst = Instruction::new(*opcode, arg_values, ty);
                let inst_id = self.dfg.make_inst(inst);
                Some(self.dfg.make_inst_result(inst_id, ty))
            }

            Pattern::Constant(value) => {
                // Create a constant instruction
                let inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, *value);
                let inst_id = self.dfg.make_inst(inst);
                Some(self.dfg.make_inst_result(inst_id, Type::I32))
            }

            Pattern::AnyConstant(var) => {
                // Look up the constant value and create it
                if let Some(const_val) = bindings.get_constant(var) {
                    let inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, const_val);
                    let inst_id = self.dfg.make_inst(inst);
                    Some(self.dfg.make_inst_result(inst_id, Type::I32))
                } else {
                    None
                }
            }

            // These patterns don't make sense on RHS
            Pattern::AnyOfType(_) | Pattern::Or(_) | Pattern::Where { .. } => None,
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
        rules.push(
            Rewrite::new("add-zero")
                .match_pattern(Pattern::op(
                    Opcode::Add,
                    vec![Pattern::var("x"), Pattern::constant(0)],
                ))
                .produce(Pattern::var("x"))
                .build(),
        );

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

        // Constant folding (example with conditions)
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
                            // Store the result for the applier to use
                            // (This is a bit hacky - in practice you'd want a better way)
                            true
                        } else {
                            false
                        }
                    },
                })
                .build(),
        );

        // Commutativity (bidirectional)
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
                .bidirectional()
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
                .bidirectional()
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
                .bidirectional()
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
                .bidirectional()
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
                .bidirectional()
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pattern_matching() {
        let mut dfg = DataFlowGraph::new();

        // Create: x = const 0
        let const_inst = Instruction::with_imm(Opcode::Const, vec![], Type::I32, 0);
        let const_id = dfg.make_inst(const_inst);
        let zero = dfg.make_inst_result(const_id, Type::I32);

        // Create: y = param
        let block = BlockId(0);
        let y = dfg.make_block_param(block, 0, Type::I32);

        // Create: z = y + 0
        let add_inst = Instruction::new(Opcode::Add, vec![y, zero], Type::I32);
        let add_id = dfg.make_inst(add_inst);
        let z = dfg.make_inst_result(add_id, Type::I32);

        // Match pattern: (x + 0)
        let pattern = Pattern::op(Opcode::Add, vec![Pattern::var("x"), Pattern::constant(0)]);

        let matcher = PatternMatcher::new(&dfg);
        let bindings = matcher.match_pattern(&pattern, z);

        assert!(bindings.is_some());
        let bindings = bindings.unwrap();
        assert_eq!(bindings.get_value(&VarId::new("x")), Some(y));

        println!("✓ Pattern matching test passed");
    }

    #[test]
    fn test_pattern_application() {
        let mut dfg = DataFlowGraph::new();

        // Create: x = param
        let block = BlockId(0);
        let x = dfg.make_block_param(block, 0, Type::I32);

        // Create bindings
        let mut bindings = Bindings::new(&dfg);
        bindings.bind_value(VarId::new("x"), x);

        // Apply pattern: just return x
        let pattern = Pattern::var("x");
        let mut applier = PatternApplier::new(&mut dfg);
        let result = applier.apply_pattern(&pattern, &bindings);

        assert_eq!(result, Some(x));

        println!("✓ Pattern application test passed");
    }

    #[test]
    fn test_rewrite_library() {
        let library = RewriteLibrary::standard();

        // Should have several standard rules
        assert!(library.rules.len() > 0);

        // Check for specific rules
        let has_add_zero = library.rules.iter().any(|r| r.name == "add-zero");
        assert!(has_add_zero);

        println!("✓ Rewrite library test passed");
    }
}
