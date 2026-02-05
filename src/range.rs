//! Range Analysis for Integer Values
//!
//! This module provides range tracking for integer values, allowing the egraph
//! pass to track and utilize assumptions about value bounds as it traverses
//! the control flow graph.
//!
//! The key data structure is `RangeAssumptions`, which provides scoped storage
//! for range facts. As the pass enters a new scope (e.g., the true branch of
//! a conditional), it can push new assumptions. When leaving that scope, the
//! assumptions are automatically popped.
//!
//! Example:
//! ```ignore
//! // if x < 10 { ... }
//! //
//! // In the true branch, we know x is in [-inf, 9]
//! assumptions.push_scope();
//! assumptions.add_upper_bound(x, 9);
//! // ... process true branch ...
//! assumptions.pop_scope();
//! ```

use crate::types::ValueId;
use std::collections::HashMap;

/// Represents a range of integer values [min, max].
///
/// Both bounds are inclusive. Use `i64::MIN` and `i64::MAX` to represent
/// unbounded ranges.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Range {
    /// Minimum value (inclusive)
    pub min: i64,
    /// Maximum value (inclusive)
    pub max: i64,
}

impl Range {
    /// Create a new range [min, max]
    pub fn new(min: i64, max: i64) -> Self {
        debug_assert!(min <= max, "Invalid range: min > max");
        Self { min, max }
    }

    /// Create an empty range (represents impossible/contradiction)
    ///
    /// An empty range has min > max, which is used to indicate
    /// that no values satisfy the constraints (dead code).
    pub fn empty() -> Self {
        Self { min: 1, max: 0 }
    }

    /// Create an unbounded range (full range of i64)
    pub fn unbounded() -> Self {
        Self {
            min: i64::MIN,
            max: i64::MAX,
        }
    }

    /// Create a range containing a single value
    pub fn singleton(value: i64) -> Self {
        Self {
            min: value,
            max: value,
        }
    }

    /// Create a range [0, max] (non-negative values)
    pub fn non_negative() -> Self {
        Self { min: 0, max: i64::MAX }
    }

    /// Create a range [1, max] (positive values)
    pub fn positive() -> Self {
        Self { min: 1, max: i64::MAX }
    }

    /// Create a range [min, -1] (negative values)
    pub fn negative() -> Self {
        Self {
            min: i64::MIN,
            max: -1,
        }
    }

    /// Create a range [min, 0] (non-positive values)
    pub fn non_positive() -> Self {
        Self { min: i64::MIN, max: 0 }
    }

    /// Check if this range contains a specific value
    pub fn contains(&self, value: i64) -> bool {
        value >= self.min && value <= self.max
    }

    /// Check if this range is a singleton (contains exactly one value)
    pub fn is_singleton(&self) -> bool {
        self.min == self.max
    }

    /// Get the singleton value, if this is a singleton range
    pub fn as_singleton(&self) -> Option<i64> {
        if self.is_singleton() {
            Some(self.min)
        } else {
            None
        }
    }

    /// Check if this range is empty (impossible)
    pub fn is_empty(&self) -> bool {
        self.min > self.max
    }

    /// Check if this range is unbounded (full i64 range)
    pub fn is_unbounded(&self) -> bool {
        self.min == i64::MIN && self.max == i64::MAX
    }

    /// Check if all values in this range are non-negative
    pub fn is_non_negative(&self) -> bool {
        self.min >= 0
    }

    /// Check if all values in this range are positive
    pub fn is_positive(&self) -> bool {
        self.min > 0
    }

    /// Check if all values in this range are negative
    pub fn is_negative(&self) -> bool {
        self.max < 0
    }

    /// Check if all values in this range are non-positive
    pub fn is_non_positive(&self) -> bool {
        self.max <= 0
    }

    /// Intersect this range with another, returning the overlap
    ///
    /// Returns None if the ranges don't overlap
    pub fn intersect(&self, other: &Range) -> Option<Range> {
        let min = self.min.max(other.min);
        let max = self.max.min(other.max);

        if min <= max {
            Some(Range { min, max })
        } else {
            None
        }
    }

    /// Union this range with another, returning the smallest range
    /// that contains both
    pub fn union(&self, other: &Range) -> Range {
        Range {
            min: self.min.min(other.min),
            max: self.max.max(other.max),
        }
    }

    /// Widen the range by constraining to an upper bound
    /// Returns the intersection with [-inf, upper]
    pub fn with_upper_bound(&self, upper: i64) -> Option<Range> {
        self.intersect(&Range {
            min: i64::MIN,
            max: upper,
        })
    }

    /// Widen the range by constraining to a lower bound
    /// Returns the intersection with [lower, +inf]
    pub fn with_lower_bound(&self, lower: i64) -> Option<Range> {
        self.intersect(&Range {
            min: lower,
            max: i64::MAX,
        })
    }

    /// Compute the range of (self + other)
    pub fn add(&self, other: &Range) -> Range {
        Range {
            min: self.min.saturating_add(other.min),
            max: self.max.saturating_add(other.max),
        }
    }

    /// Compute the range of (self - other)
    pub fn sub(&self, other: &Range) -> Range {
        Range {
            min: self.min.saturating_sub(other.max),
            max: self.max.saturating_sub(other.min),
        }
    }

    /// Compute the range of -self (negation)
    pub fn neg(&self) -> Range {
        // Note: negating i64::MIN overflows, so we need to handle it
        let new_min = if self.max == i64::MIN {
            i64::MAX // -MIN would overflow, clamp to MAX
        } else {
            self.max.saturating_neg()
        };

        let new_max = if self.min == i64::MIN {
            i64::MAX
        } else {
            self.min.saturating_neg()
        };

        Range {
            min: new_min.min(new_max),
            max: new_min.max(new_max),
        }
    }

    /// Compute the range of (self * other) - simplified version
    /// that handles common cases
    pub fn mul(&self, other: &Range) -> Range {
        // Compute all four corner products and take min/max
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

    /// Check if this range is definitely less than a value
    pub fn definitely_less_than(&self, value: i64) -> bool {
        self.max < value
    }

    /// Check if this range is definitely less than or equal to a value
    pub fn definitely_le(&self, value: i64) -> bool {
        self.max <= value
    }

    /// Check if this range is definitely greater than a value
    pub fn definitely_greater_than(&self, value: i64) -> bool {
        self.min > value
    }

    /// Check if this range is definitely greater than or equal to a value
    pub fn definitely_ge(&self, value: i64) -> bool {
        self.min >= value
    }

    /// Check if this range definitely equals a value (is a singleton of that value)
    pub fn definitely_equals(&self, value: i64) -> bool {
        self.min == value && self.max == value
    }

    /// Check if this range definitely does not equal a value
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

/// Scoped storage for range assumptions about values.
///
/// This structure maintains a stack of scopes. Each scope can contain
/// range assumptions about values. When a scope is popped, all assumptions
/// made in that scope are removed.
///
/// This is useful for tracking assumptions that are valid only in certain
/// control flow paths (e.g., after a conditional branch).
#[derive(Debug, Clone)]
pub struct RangeAssumptions {
    /// Stack of scope markers and assumptions
    /// Each entry is (scope_depth, value_id, range)
    assumptions: Vec<(usize, ValueId, Range)>,

    /// Current scope depth
    depth: usize,

    /// Cache of current ranges for each value (intersection of all assumptions)
    /// This is rebuilt when queried after modifications
    cache: HashMap<ValueId, Range>,

    /// Whether the cache is valid
    cache_valid: bool,
}

impl RangeAssumptions {
    /// Create a new empty assumptions store
    pub fn new() -> Self {
        Self {
            assumptions: Vec::new(),
            depth: 0,
            cache: HashMap::new(),
            cache_valid: true,
        }
    }

    /// Push a new scope. Assumptions added after this can be popped later.
    pub fn push_scope(&mut self) {
        self.depth += 1;
    }

    /// Pop the current scope, removing all assumptions made in it.
    pub fn pop_scope(&mut self) {
        assert!(self.depth > 0, "Cannot pop from depth 0");

        // Remove all assumptions at the current depth
        while self
            .assumptions
            .last()
            .map(|(d, _, _)| *d == self.depth)
            .unwrap_or(false)
        {
            self.assumptions.pop();
        }

        self.depth -= 1;
        self.cache_valid = false;
    }

    /// Get the current scope depth
    pub fn depth(&self) -> usize {
        self.depth
    }

    /// Add an assumption that a value is within a given range.
    ///
    /// If the value already has assumptions, this intersects with the
    /// existing range. If the intersection is empty, the assumption is
    /// still recorded (this can be used to detect dead code).
    pub fn assume_range(&mut self, value: ValueId, range: Range) {
        self.assumptions.push((self.depth, value, range));
        self.cache_valid = false;
    }

    /// Add an assumption that a value is at least `lower`
    pub fn assume_lower_bound(&mut self, value: ValueId, lower: i64) {
        self.assume_range(value, Range::new(lower, i64::MAX));
    }

    /// Add an assumption that a value is at most `upper`
    pub fn assume_upper_bound(&mut self, value: ValueId, upper: i64) {
        self.assume_range(value, Range::new(i64::MIN, upper));
    }

    /// Add an assumption that a value equals a specific constant
    pub fn assume_equals(&mut self, value: ValueId, constant: i64) {
        self.assume_range(value, Range::singleton(constant));
    }

    /// Add an assumption that a value is non-negative (>= 0)
    pub fn assume_non_negative(&mut self, value: ValueId) {
        self.assume_range(value, Range::non_negative());
    }

    /// Add an assumption that a value is positive (> 0)
    pub fn assume_positive(&mut self, value: ValueId) {
        self.assume_range(value, Range::positive());
    }

    /// Add an assumption that a value is negative (< 0)
    pub fn assume_negative(&mut self, value: ValueId) {
        self.assume_range(value, Range::negative());
    }

    /// Get the current range assumption for a value, if any.
    ///
    /// This computes the intersection of all assumptions about this value
    /// across all active scopes.
    pub fn get_range(&mut self, value: ValueId) -> Range {
        self.rebuild_cache_if_needed();
        self.cache.get(&value).copied().unwrap_or(Range::unbounded())
    }

    /// Check if we have any assumptions about a value
    pub fn has_assumptions(&self, value: ValueId) -> bool {
        self.assumptions.iter().any(|(_, v, _)| *v == value)
    }

    /// Check if a value is known to be in a specific range
    pub fn is_in_range(&mut self, value: ValueId, min: i64, max: i64) -> bool {
        let range = self.get_range(value);
        range.min >= min && range.max <= max
    }

    /// Check if a value is known to be non-negative
    pub fn is_non_negative(&mut self, value: ValueId) -> bool {
        self.get_range(value).is_non_negative()
    }

    /// Check if a value is known to be positive
    pub fn is_positive(&mut self, value: ValueId) -> bool {
        self.get_range(value).is_positive()
    }

    /// Check if a value is known to be a specific constant
    pub fn is_constant(&mut self, value: ValueId) -> Option<i64> {
        self.get_range(value).as_singleton()
    }

    /// Check if the assumptions for a value are contradictory (empty range)
    ///
    /// This indicates dead code - the current path is impossible.
    pub fn is_unreachable(&mut self, value: ValueId) -> bool {
        self.rebuild_cache_if_needed();
        self.cache.get(&value).map(|r| r.is_empty()).unwrap_or(false)
    }

    /// Rebuild the cache by intersecting all assumptions
    fn rebuild_cache_if_needed(&mut self) {
        if self.cache_valid {
            return;
        }

        self.cache.clear();

        for (_, value, range) in &self.assumptions {
            let entry = self.cache.entry(*value).or_insert(Range::unbounded());
            if let Some(intersected) = entry.intersect(range) {
                *entry = intersected;
            } else {
                // Ranges don't overlap - mark as empty (contradiction)
                // Use a special empty range representation (min > max)
                *entry = Range::empty();
            }
        }

        self.cache_valid = true;
    }

    /// Clear all assumptions
    pub fn clear(&mut self) {
        self.assumptions.clear();
        self.cache.clear();
        self.cache_valid = true;
        self.depth = 0;
    }

    /// Get statistics about the current assumptions
    pub fn stats(&self) -> RangeAssumptionsStats {
        RangeAssumptionsStats {
            total_assumptions: self.assumptions.len(),
            current_depth: self.depth,
            unique_values: self
                .assumptions
                .iter()
                .map(|(_, v, _)| v)
                .collect::<std::collections::HashSet<_>>()
                .len(),
        }
    }
}

impl Default for RangeAssumptions {
    fn default() -> Self {
        Self::new()
    }
}

/// Statistics about range assumptions
#[derive(Debug, Clone)]
pub struct RangeAssumptionsStats {
    pub total_assumptions: usize,
    pub current_depth: usize,
    pub unique_values: usize,
}

/// Learn range facts from comparison instructions.
///
/// Given a comparison instruction that is true/false, learn what range
/// facts we can deduce about the operands.
pub fn learn_from_comparison(
    opcode: crate::types::Opcode,
    lhs: ValueId,
    rhs_constant: Option<i64>,
    is_true_branch: bool,
) -> Vec<(ValueId, Range)> {
    use crate::types::Opcode;

    let mut facts = Vec::new();

    // Only handle comparisons with known constant on RHS for now
    let Some(rhs) = rhs_constant else {
        return facts;
    };

    match (opcode, is_true_branch) {
        // x < rhs (true) => x in [-inf, rhs-1]
        (Opcode::Slt, true) | (Opcode::Ult, true) => {
            if rhs > i64::MIN {
                facts.push((lhs, Range::new(i64::MIN, rhs - 1)));
            }
        }
        // x < rhs (false) => x in [rhs, +inf]
        (Opcode::Slt, false) | (Opcode::Ult, false) => {
            facts.push((lhs, Range::new(rhs, i64::MAX)));
        }

        // x <= rhs (true) => x in [-inf, rhs]
        (Opcode::Sle, true) | (Opcode::Ule, true) => {
            facts.push((lhs, Range::new(i64::MIN, rhs)));
        }
        // x <= rhs (false) => x in [rhs+1, +inf]
        (Opcode::Sle, false) | (Opcode::Ule, false) => {
            if rhs < i64::MAX {
                facts.push((lhs, Range::new(rhs + 1, i64::MAX)));
            }
        }

        // x > rhs (true) => x in [rhs+1, +inf]
        (Opcode::Sgt, true) | (Opcode::Ugt, true) => {
            if rhs < i64::MAX {
                facts.push((lhs, Range::new(rhs + 1, i64::MAX)));
            }
        }
        // x > rhs (false) => x in [-inf, rhs]
        (Opcode::Sgt, false) | (Opcode::Ugt, false) => {
            facts.push((lhs, Range::new(i64::MIN, rhs)));
        }

        // x >= rhs (true) => x in [rhs, +inf]
        (Opcode::Sge, true) | (Opcode::Uge, true) => {
            facts.push((lhs, Range::new(rhs, i64::MAX)));
        }
        // x >= rhs (false) => x in [-inf, rhs-1]
        (Opcode::Sge, false) | (Opcode::Uge, false) => {
            if rhs > i64::MIN {
                facts.push((lhs, Range::new(i64::MIN, rhs - 1)));
            }
        }

        // x == rhs (true) => x in [rhs, rhs]
        (Opcode::Eq, true) => {
            facts.push((lhs, Range::singleton(rhs)));
        }
        // x == rhs (false) => we can't narrow much without disjoint ranges

        // x != rhs (true) => we can't narrow much
        // x != rhs (false) => x in [rhs, rhs]
        (Opcode::Ne, false) => {
            facts.push((lhs, Range::singleton(rhs)));
        }

        _ => {}
    }

    facts
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_range_basics() {
        let r = Range::new(0, 10);
        assert!(r.contains(0));
        assert!(r.contains(5));
        assert!(r.contains(10));
        assert!(!r.contains(-1));
        assert!(!r.contains(11));
    }

    #[test]
    fn test_range_singleton() {
        let r = Range::singleton(42);
        assert!(r.is_singleton());
        assert_eq!(r.as_singleton(), Some(42));
        assert!(r.contains(42));
        assert!(!r.contains(41));
    }

    #[test]
    fn test_range_intersection() {
        let r1 = Range::new(0, 10);
        let r2 = Range::new(5, 15);
        let r3 = Range::new(20, 30);

        assert_eq!(r1.intersect(&r2), Some(Range::new(5, 10)));
        assert_eq!(r1.intersect(&r3), None);
    }

    #[test]
    fn test_range_union() {
        let r1 = Range::new(0, 10);
        let r2 = Range::new(5, 15);

        assert_eq!(r1.union(&r2), Range::new(0, 15));
    }

    #[test]
    fn test_range_arithmetic() {
        let r1 = Range::new(1, 10);
        let r2 = Range::new(2, 5);

        // Addition
        let sum = r1.add(&r2);
        assert_eq!(sum, Range::new(3, 15));

        // Subtraction
        let diff = r1.sub(&r2);
        assert_eq!(diff, Range::new(-4, 8));
    }

    #[test]
    fn test_scoped_assumptions() {
        let mut assumptions = RangeAssumptions::new();
        let v1 = ValueId(1);
        let v2 = ValueId(2);

        // Add assumption at top level
        assumptions.assume_lower_bound(v1, 0);
        assert!(assumptions.is_non_negative(v1));

        // Push scope and add more assumptions
        assumptions.push_scope();
        assumptions.assume_upper_bound(v1, 10);
        assumptions.assume_positive(v2);

        assert_eq!(assumptions.get_range(v1), Range::new(0, 10));
        assert!(assumptions.is_positive(v2));

        // Pop scope - v2 assumption should be gone
        assumptions.pop_scope();

        // v1 still has the top-level assumption
        assert!(assumptions.is_non_negative(v1));
        // But no longer has the upper bound
        assert_eq!(assumptions.get_range(v1), Range::new(0, i64::MAX));
    }

    #[test]
    fn test_contradictory_assumptions() {
        let mut assumptions = RangeAssumptions::new();
        let v1 = ValueId(1);

        // x > 10 and x < 5 is impossible
        assumptions.assume_lower_bound(v1, 11);
        assumptions.assume_upper_bound(v1, 4);

        assert!(assumptions.is_unreachable(v1));
    }

    #[test]
    fn test_learn_from_slt() {
        use crate::types::Opcode;
        let v = ValueId(1);

        // x < 10 (true branch)
        let facts = learn_from_comparison(Opcode::Slt, v, Some(10), true);
        assert_eq!(facts.len(), 1);
        assert_eq!(facts[0].1, Range::new(i64::MIN, 9));

        // x < 10 (false branch)
        let facts = learn_from_comparison(Opcode::Slt, v, Some(10), false);
        assert_eq!(facts.len(), 1);
        assert_eq!(facts[0].1, Range::new(10, i64::MAX));
    }

    #[test]
    fn test_learn_from_eq() {
        use crate::types::Opcode;
        let v = ValueId(1);

        // x == 42 (true branch)
        let facts = learn_from_comparison(Opcode::Eq, v, Some(42), true);
        assert_eq!(facts.len(), 1);
        assert_eq!(facts[0].1, Range::singleton(42));
    }
}
