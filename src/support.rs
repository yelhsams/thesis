use crate::types::*;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

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
    /// dominator-tree children
    pub fn from_cfg(root: BlockId, edges: &[(BlockId, Vec<BlockId>)]) -> Self {
        let mut tree = Self::new();

        let children_map: HashMap<BlockId, Vec<BlockId>> = edges.iter().cloned().collect();

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

    /// Build a proper dominator tree from the CFG defined by the layout and DFG.
    pub fn from_layout(layout: &Layout, dfg: &DataFlowGraph) -> Self {
        let entry = match layout.entry_block() {
            Some(b) => b,
            None => return Self::new(),
        };

        // Step 1: Extract CFG successors
        let mut succs: HashMap<BlockId, Vec<BlockId>> = HashMap::new();
        let mut preds: HashMap<BlockId, Vec<BlockId>> = HashMap::new();

        for &block_id in &layout.blocks {
            succs.entry(block_id).or_default();
            preds.entry(block_id).or_default();
        }

        for &block_id in &layout.blocks {
            let block = &layout.block_data[&block_id];
            for &inst_id in &block.insts {
                let inst = &dfg.insts[&inst_id];
                match &inst.branch_info {
                    Some(BranchInfo::Jump(target)) => {
                        succs.entry(block_id).or_default().push(*target);
                        preds.entry(*target).or_default().push(block_id);
                    }
                    Some(BranchInfo::Conditional(then_b, _, else_b, _)) => {
                        succs.entry(block_id).or_default().push(*then_b);
                        succs.entry(block_id).or_default().push(*else_b);
                        preds.entry(*then_b).or_default().push(block_id);
                        preds.entry(*else_b).or_default().push(block_id);
                    }
                    None => {}
                }
            }
        }

        // Step 2: Compute reverse postorder (RPO) numbering via DFS
        let mut rpo_order: Vec<BlockId> = Vec::new();
        let mut visited: HashSet<BlockId> = HashSet::new();
        let mut dfs_stack: Vec<(BlockId, usize)> = vec![(entry, 0)];
        visited.insert(entry);

        while let Some((block, idx)) = dfs_stack.last_mut() {
            let block_succs = succs.get(block).cloned().unwrap_or_default();
            if *idx < block_succs.len() {
                let next = block_succs[*idx];
                *idx += 1;
                if visited.insert(next) {
                    dfs_stack.push((next, 0));
                }
            } else {
                rpo_order.push(*block);
                dfs_stack.pop();
            }
        }
        rpo_order.reverse();

        // Step 3: Build RPO numbering map
        let mut rpo_num: HashMap<BlockId, usize> = HashMap::new();
        for (i, &b) in rpo_order.iter().enumerate() {
            rpo_num.insert(b, i);
        }

        // Step 4: Cooper-Harvey-Kennedy iterative dominator algorithm
        let mut idom: HashMap<BlockId, BlockId> = HashMap::new();
        idom.insert(entry, entry);

        let intersect = |mut b1: BlockId,
                         mut b2: BlockId,
                         idom: &HashMap<BlockId, BlockId>,
                         rpo_num: &HashMap<BlockId, usize>|
         -> BlockId {
            while b1 != b2 {
                while rpo_num[&b1] > rpo_num[&b2] {
                    b1 = idom[&b1];
                }
                while rpo_num[&b2] > rpo_num[&b1] {
                    b2 = idom[&b2];
                }
            }
            b1
        };

        let mut changed = true;
        while changed {
            changed = false;
            for &b in &rpo_order {
                if b == entry {
                    continue;
                }
                let block_preds = preds.get(&b).cloned().unwrap_or_default();
                // Find first predecessor that already has an idom
                let mut new_idom: Option<BlockId> = None;
                for &p in &block_preds {
                    if idom.contains_key(&p) {
                        new_idom = Some(match new_idom {
                            Some(current) => intersect(current, p, &idom, &rpo_num),
                            None => p,
                        });
                    }
                }
                if let Some(new_idom) = new_idom {
                    if idom.get(&b) != Some(&new_idom) {
                        idom.insert(b, new_idom);
                        changed = true;
                    }
                }
            }
        }

        // Step 5: Build tree structure
        let mut tree = Self::new();
        tree.idom = idom.clone();
        // Remove self-loop for entry
        tree.idom.remove(&entry);

        for (&block, &dom) in &idom {
            if block != dom {
                tree.children.entry(dom).or_default().push(block);
            }
        }

        // Sort children for deterministic traversal order
        for children in tree.children.values_mut() {
            children.sort();
        }

        // Step 6: Compute dominator sets
        for &block in &rpo_order {
            let mut doms = HashSet::new();
            doms.insert(block);
            let mut current = block;
            while let Some(&d) = tree.idom.get(&current) {
                doms.insert(d);
                current = d;
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

    /// Return the immediate dominator
    pub fn idom_of(&self, block: BlockId) -> Option<BlockId> {
        self.idom.get(&block).copied()
    }

    /// Compute the lowest common ancestor of `a` and `b` in the dominator
    /// tree
    pub fn lca(&self, a: BlockId, b: BlockId) -> Option<BlockId> {
        if self.block_dominates(a, b) {
            return Some(a);
        }
        if self.block_dominates(b, a) {
            return Some(b);
        }
        // Walk up from `a` until we find a block that dominates `b`.
        let mut cur = a;
        for _ in 0..4096 {
            let parent = self.idom.get(&cur).copied()?;
            if parent == cur {
                return None;
            }
            if self.block_dominates(parent, b) {
                return Some(parent);
            }
            cur = parent;
        }
        None
    }
}

/// Scoped hash map for GVN (Global Value Numbering)
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

    pub fn entry(&mut self, key: K) -> ScopedEntry<'_, K, V> {
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
