use crate::types::*;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

/// Simplified dominator tree
///
/// should be computed from CFG, but now use simple interface.
#[derive(Debug, Clone)]
pub struct DominatorTree {
    /// Maps block to its immediate dominator
    idom: HashMap<BlockId, BlockId>,

    /// Maps block to its children in the domtree
    children: HashMap<BlockId, Vec<BlockId>>,

    /// Precomputed dominator sets for O(1) dominance checks
    dominators: HashMap<BlockId, HashSet<BlockId>>,
}

impl DominatorTree {
    pub fn new() -> Self {
        Self {
            idom: HashMap::new(),
            children: HashMap::new(),
            dominators: HashMap::new(),
        }
    }

    /// Builds linear dominator tree.
    pub fn from_linear_blocks(blocks: &[BlockId]) -> Self {
        let mut tree = Self::new();

        for window in blocks.windows(2) {
            let parent = window[0];
            let child = window[1];

            tree.idom.insert(child, parent);
            tree.children
                .entry(parent)
                .or_insert_with(Vec::new)
                .push(child);
        }

        // Compute dominator sets
        for &block in blocks {
            let mut doms = HashSet::new();
            doms.insert(block);

            let mut current = block;
            while let Some(&idom) = tree.idom.get(&current) {
                doms.insert(idom);
                current = idom;
            }

            tree.dominators.insert(block, doms);
        }

        tree
    }

    /// Build a dominator tree from an explicit CFG.
    ///
    /// `root` is the entry block.  `edges` maps each block to its
    /// dominator-tree *children* (not CFG successors).  This is a
    /// convenience for tests where the dominator tree shape is known.
    pub fn from_cfg(root: BlockId, edges: &[(BlockId, Vec<BlockId>)]) -> Self {
        let mut tree = Self::new();

        let children_map: HashMap<BlockId, Vec<BlockId>> = edges.iter().cloned().collect();

        // Build idom + children from the explicit tree edges
        fn walk(
            block: BlockId,
            parent: Option<BlockId>,
            children_map: &HashMap<BlockId, Vec<BlockId>>,
            tree: &mut DominatorTree,
        ) {
            if let Some(p) = parent {
                tree.idom.insert(block, p);
            }
            let kids = children_map.get(&block).cloned().unwrap_or_default();
            tree.children.insert(block, kids.clone());
            for child in &kids {
                walk(*child, Some(block), children_map, tree);
            }
        }

        walk(root, None, &children_map, &mut tree);

        // Compute dominator sets
        let all_blocks: Vec<BlockId> = edges.iter().map(|(b, _)| *b).collect();
        for &block in &all_blocks {
            let mut doms = HashSet::new();
            doms.insert(block);
            let mut current = block;
            while let Some(&idom) = tree.idom.get(&current) {
                doms.insert(idom);
                current = idom;
            }
            tree.dominators.insert(block, doms);
        }

        tree
    }

    /// Check if block `a` dominates block `b`
    pub fn block_dominates(&self, a: BlockId, b: BlockId) -> bool {
        if a == b {
            return true;
        }

        self.dominators
            .get(&b)
            .map(|doms| doms.contains(&a))
            .unwrap_or(false)
    }

    /// Get the children of a block in the dominator tree
    pub fn children(&self, block: BlockId) -> &[BlockId] {
        self.children
            .get(&block)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }
}

/// Scoped hash map for GVN (Global Value Numbering)
///
/// This is a hash map with scope levels. When you increment depth,
/// you create a new scope. Values inserted at this depth are only
/// visible at this depth and deeper. When you decrement, those
/// values become invisible again.
///
/// This is crucial for the egraph pass because it allows us to
/// do GVN that respects dominance: a value is only available
/// in blocks it dominates.
pub struct ScopedHashMap<K, V> {
    /// The underlying map
    map: HashMap<K, Vec<(usize, V)>>,

    /// Current depth (scope level)
    depth: usize,
}

impl<K: Eq + Hash + Clone, V: Clone> ScopedHashMap<K, V> {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            depth: 0,
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            map: HashMap::with_capacity(capacity),
            depth: 0,
        }
    }

    /// Increment scope depth
    pub fn increment_depth(&mut self) {
        self.depth += 1;
    }

    /// Decrement scope depth and remove entries from the old depth
    pub fn decrement_depth(&mut self) {
        assert!(self.depth > 0, "Cannot decrement depth below 0");

        // Remove all entries at the current depth
        for entries in self.map.values_mut() {
            while entries
                .last()
                .map(|(d, _)| *d == self.depth)
                .unwrap_or(false)
            {
                entries.pop();
            }
        }

        self.depth -= 1;
    }

    /// Get a value at the current depth or any ancestor scope
    pub fn get(&self, key: &K) -> Option<&V> {
        self.map
            .get(key)
            .and_then(|entries| entries.last())
            .map(|(_, v)| v)
    }

    /// Insert a value at the current depth
    pub fn insert(&mut self, key: K, value: V) {
        self.map
            .entry(key)
            .or_insert_with(Vec::new)
            .push((self.depth, value));
    }

    /// Insert a value at a specific depth (used for LICM-style hoisting)
    pub fn insert_at_depth(&mut self, key: K, value: V, depth: usize) {
        assert!(
            depth <= self.depth,
            "Cannot insert at depth > current depth"
        );
        self.map
            .entry(key)
            .or_insert_with(Vec::new)
            .push((depth, value));
    }

    /// Entry API for scoped hash map
    pub fn entry(&mut self, key: K) -> ScopedEntry<K, V> {
        if self.get(&key).is_some() {
            ScopedEntry::Occupied(OccupiedEntry { map: self, key })
        } else {
            ScopedEntry::Vacant(VacantEntry { map: self, key })
        }
    }
}

pub enum ScopedEntry<'a, K, V> {
    Occupied(OccupiedEntry<'a, K, V>),
    Vacant(VacantEntry<'a, K, V>),
}

pub struct OccupiedEntry<'a, K, V> {
    map: &'a ScopedHashMap<K, V>,
    key: K,
}

impl<'a, K: Eq + Hash + Clone, V: Clone> OccupiedEntry<'a, K, V> {
    pub fn get(&self) -> &V {
        self.map.get(&self.key).unwrap()
    }
}

pub struct VacantEntry<'a, K, V> {
    map: &'a mut ScopedHashMap<K, V>,
    key: K,
}

impl<'a, K: Eq + Hash + Clone, V: Clone> VacantEntry<'a, K, V> {
    pub fn insert(self, value: V) {
        self.map.insert(self.key, value);
    }
}

/// Simple alias map for value canonicalization
#[derive(Debug, Clone)]
pub struct ValueAliases {
    /// Maps value to its canonical representative
    /// If not present, value is its own canonical form
    aliases: HashMap<ValueId, ValueId>,
}

impl ValueAliases {
    pub fn new() -> Self {
        Self {
            aliases: HashMap::new(),
        }
    }

    /// Find the canonical representative (non-mutating)
    pub fn resolve(&self, mut value: ValueId) -> ValueId {
        // Follow alias chain to canonical value
        while let Some(&next) = self.aliases.get(&value) {
            if next == value {
                break; // Prevent infinite loops
            }
            value = next;
        }
        value
    }

    /// Alias one value to another (always making b canonical)
    pub fn union(&mut self, a: ValueId, b: ValueId) {
        let a_canonical = self.resolve(a);
        let b_canonical = self.resolve(b);

        if a_canonical != b_canonical {
            // Make b the canonical representative
            self.aliases.insert(a_canonical, b_canonical);
        }
    }
}
