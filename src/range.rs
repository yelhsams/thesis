//! Range analysis for integer values by adding assumptions
//! into/outof scope

use crate::types::ValueId;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Range {
    // inclusive
    pub min: i64,
    pub max: i64,
}

impl Range {
    pub fn new(min: i64, max: i64) -> Self {
        debug_assert!(min <= max, "Invalid range: min > max");
        Self { min, max }
    }

    pub fn empty() -> Self {
        Self { min: 1, max: 0 }
    }

    // unbounded
    pub fn unbounded() -> Self {
        Self {
            min: i64::MIN,
            max: i64::MAX,
        }
    }

    // deduced single value
    pub fn singleton(value: i64) -> Self {
        Self {
            min: value,
            max: value,
        }
    }

    pub fn non_negative() -> Self {
        Self {
            min: 0,
            max: i64::MAX,
        }
    }

    pub fn non_positive() -> Self {
        Self {
            min: i64::MIN,
            max: 0,
        }
    }

    pub fn positive() -> Self {
        Self {
            min: 1,
            max: i64::MAX,
        }
    }

    pub fn negative() -> Self {
        Self {
            min: i64::MIN,
            max: -1,
        }
    }

    // Check if this range contains a specific value
    pub fn contains(&self, value: i64) -> bool {
        value >= self.min && value <= self.max
    }

    pub fn is_singleton(&self) -> bool {
        self.min == self.max
    }

    pub fn as_singleton(&self) -> Option<i64> {
        if self.is_singleton() {
            Some(self.min)
        } else {
            None
        }
    }

    pub fn is_empty(&self) -> bool {
        self.min > self.max
    }

    pub fn is_unbounded(&self) -> bool {
        self.min == i64::MIN && self.max == i64::MAX
    }

    pub fn is_non_negative(&self) -> bool {
        self.min >= 0
    }

    pub fn is_non_positive(&self) -> bool {
        self.max <= 0
    }

    pub fn is_positive(&self) -> bool {
        self.min > 0
    }

    pub fn is_negative(&self) -> bool {
        self.max < 0
    }

    // Intersect this range with another, returning the overlap
    // None if no overlap
    pub fn intersect(&self, other: &Range) -> Option<Range> {
        let min = self.min.max(other.min);
        let max = self.max.min(other.max);

        if min <= max {
            Some(Range { min, max })
        } else {
            None
        }
    }

    pub fn union(&self, other: &Range) -> Range {
        Range {
            min: self.min.min(other.min),
            max: self.max.max(other.max),
        }
    }

    pub fn definitely_less_than(&self, value: i64) -> bool {
        self.max < value
    }

    pub fn definitely_le(&self, value: i64) -> bool {
        self.max <= value
    }

    pub fn definitely_greater_than(&self, value: i64) -> bool {
        self.min > value
    }

    pub fn definitely_ge(&self, value: i64) -> bool {
        self.min >= value
    }

    pub fn definitely_equals(&self, value: i64) -> bool {
        self.min == value && self.max == value
    }

    pub fn definitely_not_equals(&self, value: i64) -> bool {
        self.max < value || self.min > value
    }

    // --- Range arithmetic (transfer functions) ---

    /// Compute the range of (self + other), saturating on overflow.
    pub fn add(&self, other: &Range) -> Range {
        Range {
            min: self.min.saturating_add(other.min),
            max: self.max.saturating_add(other.max),
        }
    }

    /// Compute the range of (self - other), saturating on overflow.
    pub fn sub(&self, other: &Range) -> Range {
        Range {
            min: self.min.saturating_sub(other.max),
            max: self.max.saturating_sub(other.min),
        }
    }

    /// Compute the range of (self * other), saturating on overflow.
    pub fn mul(&self, other: &Range) -> Range {
        let products = [
            self.min.saturating_mul(other.min),
            self.min.saturating_mul(other.max),
            self.max.saturating_mul(other.min),
            self.max.saturating_mul(other.max),
        ];
        Range {
            min: *products.iter().min().unwrap(),
            max: *products.iter().max().unwrap(),
        }
    }

    /// Compute the range of a bitwise AND. Conservative: if both non-negative,
    /// result is in [0, min(self.max, other.max)].
    pub fn bitand(&self, other: &Range) -> Range {
        if self.is_non_negative() && other.is_non_negative() {
            Range::new(0, self.max.min(other.max))
        } else {
            Range::unbounded()
        }
    }

    /// Compute the range of a bitwise OR. Conservative.
    pub fn bitor(&self, other: &Range) -> Range {
        if self.is_non_negative() && other.is_non_negative() {
            // Upper bound: next power of two above max of both, minus 1
            let upper = self.max.max(other.max) as u64;
            let bound = upper
                .checked_next_power_of_two()
                .unwrap_or(u64::MAX)
                .saturating_sub(1)
                .max(upper);
            Range::new(0, bound.min(i64::MAX as u64) as i64)
        } else {
            Range::unbounded()
        }
    }

    /// Compute the range of a negation (-self).
    pub fn neg(&self) -> Range {
        // -[a, b] = [-b, -a], with saturation for i64::MIN
        Range {
            min: 0i64.saturating_sub(self.max),
            max: 0i64.saturating_sub(self.min),
        }
    }

    /// Compute the range of a left shift by a constant amount.
    pub fn shl_const(&self, amount: i64) -> Range {
        if amount < 0 || amount >= 63 {
            return Range::unbounded();
        }
        Range {
            min: self.min.saturating_mul(1i64.wrapping_shl(amount as u32)),
            max: self.max.saturating_mul(1i64.wrapping_shl(amount as u32)),
        }
    }

    /// Compute the range of an arithmetic right shift by a constant.
    pub fn sshr_const(&self, amount: i64) -> Range {
        if amount < 0 || amount >= 63 {
            return Range::unbounded();
        }
        let shift = amount as u32;
        Range {
            min: self.min >> shift,
            max: self.max >> shift,
        }
    }
}

impl Default for Range {
    fn default() -> Self {
        Self::unbounded()
    }
}

impl std::fmt::Display for Range {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_singleton() {
            write!(f, "[{}]", self.min)
        } else if self.min == i64::MIN && self.max == i64::MAX {
            write!(f, "[-inf, +inf]")
        } else if self.min == i64::MIN {
            write!(f, "[-inf, {}]", self.max)
        } else if self.max == i64::MAX {
            write!(f, "[{}, +inf]", self.min)
        } else {
            write!(f, "[{}, {}]", self.min, self.max)
        }
    }
}

// notes:
// sketch out idea first.
// run into difficulties I didn't anticipate
// pushing/popping basic assumptions
// whether rules should match
// union nodes need to be tagged under what context

// understand what order we are puting into the egraph, what order we are visiting thems

// start with small example
// super simple with partially broken algorithm
// examples that would break if assumptions 'lived too long'

// focus on order of visits

// draw control flow graph to side

// sightglass

// tree based cost model recursively

#[derive(Debug, Clone)]
pub struct RangeAssumptions {
    scopes: Vec<HashMap<ValueId, Range>>,
}

impl RangeAssumptions {
    /// Create a new empty assumptions store with one root scope.
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn push_scope(&mut self) {
        let snapshot = self.scopes.last().cloned().unwrap_or_default();
        self.scopes.push(snapshot);
    }

    pub fn pop_scope(&mut self) {
        assert!(self.scopes.len() > 1, "Cannot pop the root scope");
        self.scopes.pop();
    }

    pub fn depth(&self) -> usize {
        self.scopes.len() - 1
    }

    pub fn assume_range(&mut self, value: ValueId, range: Range) {
        let frame = self.scopes.last_mut().expect("at least one scope");
        let current = frame.entry(value).or_insert(Range::unbounded());
        *current = match current.intersect(&range) {
            Some(r) => r,
            None => Range::empty(), // contradiction
        };
    }

    pub fn assume_lower_bound(&mut self, value: ValueId, lower: i64) {
        self.assume_range(value, Range::new(lower, i64::MAX));
    }

    pub fn assume_upper_bound(&mut self, value: ValueId, upper: i64) {
        self.assume_range(value, Range::new(i64::MIN, upper));
    }

    pub fn assume_equals(&mut self, value: ValueId, constant: i64) {
        self.assume_range(value, Range::singleton(constant));
    }

    pub fn assume_non_negative(&mut self, value: ValueId) {
        self.assume_range(value, Range::non_negative());
    }

    pub fn assume_non_positive(&mut self, value: ValueId) {
        self.assume_range(value, Range::non_positive());
    }

    pub fn assume_negative(&mut self, value: ValueId) {
        self.assume_range(value, Range::negative());
    }

    pub fn assume_positive(&mut self, value: ValueId) {
        self.assume_range(value, Range::positive());
    }

    /// Get the current (tightest known) range for a value.
    pub fn get_range(&self, value: ValueId) -> Range {
        self.scopes
            .last()
            .expect("at least one scope")
            .get(&value)
            .copied()
            .unwrap_or(Range::unbounded())
    }

    pub fn has_assumptions(&self, value: ValueId) -> bool {
        self.scopes
            .last()
            .map(|f| f.contains_key(&value))
            .unwrap_or(false)
    }

    pub fn is_in_range(&self, value: ValueId, min: i64, max: i64) -> bool {
        let r = self.get_range(value);
        r.min >= min && r.max <= max
    }

    pub fn is_non_negative(&self, value: ValueId) -> bool {
        self.get_range(value).is_non_negative()
    }

    pub fn is_non_positive(&self, value: ValueId) -> bool {
        self.get_range(value).is_non_positive()
    }

    pub fn is_negative(&self, value: ValueId) -> bool {
        self.get_range(value).is_negative()
    }

    pub fn is_positive(&self, value: ValueId) -> bool {
        self.get_range(value).is_positive()
    }

    pub fn is_constant(&self, value: ValueId) -> Option<i64> {
        self.get_range(value).as_singleton()
    }

    pub fn is_unreachable(&self, value: ValueId) -> bool {
        self.get_range(value).is_empty()
    }

    pub fn clear(&mut self) {
        self.scopes.clear();
        self.scopes.push(HashMap::new());
    }

    pub fn stats(&self) -> RangeAssumptionsStats {
        let frame = self.scopes.last().expect("at least one scope");
        RangeAssumptionsStats {
            total_assumptions: frame.len(),
            current_depth: self.depth(),
            unique_values: frame.len(),
        }
    }
}

impl Default for RangeAssumptions {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct RangeAssumptionsStats {
    pub total_assumptions: usize,
    pub current_depth: usize,
    pub unique_values: usize,
}

/// Compute the output range of an instruction given the ranges of its inputs.
///
/// This is the core transfer function that propagates range information
/// forward through arithmetic and logical operations.
pub fn compute_inst_range(
    opcode: crate::types::Opcode,
    arg_ranges: &[Range],
    immediate: Option<i64>,
) -> Range {
    use crate::types::Opcode;

    match opcode {
        Opcode::Const => {
            if let Some(imm) = immediate {
                Range::singleton(imm)
            } else {
                Range::unbounded()
            }
        }

        Opcode::Add => {
            if arg_ranges.len() == 2 {
                arg_ranges[0].add(&arg_ranges[1])
            } else {
                Range::unbounded()
            }
        }

        Opcode::Sub => {
            if arg_ranges.len() == 2 {
                arg_ranges[0].sub(&arg_ranges[1])
            } else {
                Range::unbounded()
            }
        }

        Opcode::Mul => {
            if arg_ranges.len() == 2 {
                arg_ranges[0].mul(&arg_ranges[1])
            } else {
                Range::unbounded()
            }
        }

        Opcode::And => {
            if arg_ranges.len() == 2 {
                arg_ranges[0].bitand(&arg_ranges[1])
            } else {
                Range::unbounded()
            }
        }

        Opcode::Or => {
            if arg_ranges.len() == 2 {
                arg_ranges[0].bitor(&arg_ranges[1])
            } else {
                Range::unbounded()
            }
        }

        Opcode::Ineg => {
            if arg_ranges.len() == 1 {
                arg_ranges[0].neg()
            } else {
                Range::unbounded()
            }
        }

        Opcode::Shl => {
            if arg_ranges.len() == 2 {
                if let Some(amt) = arg_ranges[1].as_singleton() {
                    arg_ranges[0].shl_const(amt)
                } else {
                    Range::unbounded()
                }
            } else {
                Range::unbounded()
            }
        }

        Opcode::Sshr => {
            if arg_ranges.len() == 2 {
                if let Some(amt) = arg_ranges[1].as_singleton() {
                    arg_ranges[0].sshr_const(amt)
                } else {
                    Range::unbounded()
                }
            } else {
                Range::unbounded()
            }
        }

        // Comparisons always produce 0 or 1
        Opcode::Eq | Opcode::Ne | Opcode::Slt | Opcode::Sle | Opcode::Sgt | Opcode::Sge
        | Opcode::Ult | Opcode::Ule | Opcode::Ugt | Opcode::Uge => Range::new(0, 1),

        // Division: conservative, but if divisor is a known positive constant
        // and dividend is non-negative, we can narrow the range.
        Opcode::Div => {
            if arg_ranges.len() == 2 {
                if let Some(divisor) = arg_ranges[1].as_singleton() {
                    if divisor > 0 && arg_ranges[0].is_non_negative() {
                        Range::new(
                            arg_ranges[0].min / divisor,
                            arg_ranges[0].max / divisor,
                        )
                    } else if divisor != 0 {
                        Range::unbounded()
                    } else {
                        Range::unbounded()
                    }
                } else {
                    Range::unbounded()
                }
            } else {
                Range::unbounded()
            }
        }

        _ => Range::unbounded(),
    }
}

pub fn learn_from_comparison(
    opcode: crate::types::Opcode,
    lhs: ValueId,
    rhs_constant: Option<i64>,
    is_true_branch: bool,
) -> Vec<(ValueId, Range)> {
    use crate::types::Opcode;

    let mut facts = Vec::new();

    let Some(rhs) = rhs_constant else {
        return facts;
    };

    match (opcode, is_true_branch) {
        (Opcode::Slt, true) | (Opcode::Ult, true) => {
            if rhs > i64::MIN {
                facts.push((lhs, Range::new(i64::MIN, rhs - 1)));
            }
        }

        (Opcode::Slt, false) | (Opcode::Ult, false) => {
            facts.push((lhs, Range::new(rhs, i64::MAX)));
        }

        (Opcode::Sle, true) | (Opcode::Ule, true) => {
            facts.push((lhs, Range::new(i64::MIN, rhs)));
        }

        (Opcode::Sle, false) | (Opcode::Ule, false) => {
            if rhs < i64::MAX {
                facts.push((lhs, Range::new(rhs + 1, i64::MAX)));
            }
        }

        (Opcode::Sgt, true) | (Opcode::Ugt, true) => {
            if rhs < i64::MAX {
                facts.push((lhs, Range::new(rhs + 1, i64::MAX)));
            }
        }

        (Opcode::Sgt, false) | (Opcode::Ugt, false) => {
            facts.push((lhs, Range::new(i64::MIN, rhs)));
        }

        (Opcode::Sge, true) | (Opcode::Uge, true) => {
            facts.push((lhs, Range::new(rhs, i64::MAX)));
        }

        (Opcode::Sge, false) | (Opcode::Uge, false) => {
            if rhs > i64::MIN {
                facts.push((lhs, Range::new(i64::MIN, rhs - 1)));
            }
        }

        (Opcode::Eq, true) => {
            facts.push((lhs, Range::singleton(rhs)));
        }

        (Opcode::Ne, false) => {
            facts.push((lhs, Range::singleton(rhs)));
        }

        _ => {}
    }

    facts
}
