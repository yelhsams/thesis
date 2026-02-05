//! Range analysis for integer values by adding assumptions
//! into/outof scope

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

#[derive(Debug, Clone)]
pub struct RangeAssumptions {}
