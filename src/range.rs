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
