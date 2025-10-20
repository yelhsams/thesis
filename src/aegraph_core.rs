// Core Acyclic E-graph Implementation
// Based on Cranelift's aegraph design

use std::collections::HashMap;
use std::hash::{Hash, Hasher};

/// An eclass ID representing a snapshot of an equivalence class
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Id(u32);

impl Id {
    pub fn new(id: u32) -> Self {
        Id(id)
    }

    pub fn index(self) -> usize {
        self.0 as usize
    }
}

/// A node in the e-graph representing an operation with arguments
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Node {
    /// Operation kind (e.g., "add", "mul", "const")
    pub op: String,
    /// Arguments as eclass IDs
    pub args: Vec<Id>,
    /// Optional constant data
    pub data: Option<i64>,
}

impl Node {
    pub fn new(op: impl Into<String>, args: Vec<Id>) -> Self {
        Self {
            op: op.into(),
            args,
            data: None,
        }
    }

    pub fn with_data(op: impl Into<String>, args: Vec<Id>, data: i64) -> Self {
        Self {
            op: op.into(),
            args,
            data: Some(data),
        }
    }
}

/// An entry in the eclass table
#[derive(Debug, Clone)]
pub enum EClassEntry {
    /// A new node added to this eclass
    Node(Node),
    /// Union of two existing eclasses
    Union(Id, Id),
}

/// Union-Find data structure for canonicalization
#[derive(Debug, Clone)]
pub struct UnionFind {
    parent: Vec<Id>,
}

impl UnionFind {
    pub fn new() -> Self {
        Self { parent: Vec::new() }
    }

    /// Create a new eclass and return its ID
    pub fn make_set(&mut self) -> Id {
        let id = Id::new(self.parent.len() as u32);
        self.parent.push(id);
        id
    }

    /// Find the canonical representative with path compression
    pub fn find(&mut self, mut id: Id) -> Id {
        let mut root = id;

        // Find root
        while self.parent[root.index()] != root {
            root = self.parent[root.index()];
        }

        // Path compression
        while self.parent[id.index()] != root {
            let next = self.parent[id.index()];
            self.parent[id.index()] = root;
            id = next;
        }

        root
    }

    /// Union two eclasses, returns the canonical ID
    pub fn union(&mut self, a: Id, b: Id) -> Id {
        let a = self.find(a);
        let b = self.find(b);

        if a == b {
            return a;
        }

        // Union by rank: use smaller ID as canonical
        if a < b {
            self.parent[b.index()] = a;
            a
        } else {
            self.parent[a.index()] = b;
            b
        }
    }
}

/// The acyclic e-graph structure
pub struct AEGraph {
    /// Table of eclass entries (append-only)
    entries: Vec<EClassEntry>,

    /// Union-find for canonicalization
    uf: UnionFind,

    /// Hash-consing map: Node -> canonical eclass ID
    memo: HashMap<Node, Id>,
}

impl AEGraph {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            uf: UnionFind::new(),
            memo: HashMap::new(),
        }
    }

    /// Add a node to the e-graph, returning its eclass ID
    /// This implements the core "smart constructor" pattern
    pub fn add(&mut self, mut node: Node) -> Id {
        // Canonicalize arguments
        for arg in &mut node.args {
            *arg = self.uf.find(*arg);
        }

        // Check if node already exists (hash-consing)
        if let Some(&id) = self.memo.get(&node) {
            return self.uf.find(id);
        }

        // Create new eclass
        let id = self.uf.make_set();
        self.entries.push(EClassEntry::Node(node.clone()));
        self.memo.insert(node, id);

        // Apply rewrite rules here (in a full implementation)
        // self.apply_rules(id, &node);

        id
    }

    /// Union two eclasses
    pub fn union(&mut self, a: Id, b: Id) -> Id {
        let canonical = self.uf.union(a, b);

        // Record the union in the table
        let union_id = self.uf.make_set();
        self.entries.push(EClassEntry::Union(a, b));

        // The union node itself points to the canonical ID
        self.uf.parent[union_id.index()] = canonical;

        canonical
    }

    /// Find the canonical ID for an eclass
    pub fn find(&mut self, id: Id) -> Id {
        self.uf.find(id)
    }

    /// Get all nodes in an eclass by traversing the table
    pub fn get_nodes(&mut self, id: Id) -> Vec<Node> {
        let canonical = self.find(id);
        let mut nodes = Vec::new();

        // Traverse the entry table
        for (idx, entry) in self.entries.iter().enumerate() {
            let entry_id = Id::new(idx as u32);

            if self.find(entry_id) == canonical {
                match entry {
                    EClassEntry::Node(node) => nodes.push(node.clone()),
                    EClassEntry::Union(_, _) => {
                        // Union nodes don't add new expressions
                    }
                }
            }
        }

        nodes
    }

    /// Get the number of eclasses created
    pub fn num_eclasses(&self) -> usize {
        self.entries.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_operations() {
        let mut g = AEGraph::new();

        // Create: x + y
        let x = g.add(Node::with_data("const", vec![], 1));
        let y = g.add(Node::with_data("const", vec![], 2));
        let sum = g.add(Node::new("add", vec![x, y]));

        // Create: y + x (should be different before commutativity rule)
        let sum2 = g.add(Node::new("add", vec![y, x]));

        println!("x = {:?}, y = {:?}", x, y);
        println!("x + y = {:?}, y + x = {:?}", sum, sum2);

        // They start as different eclasses
        assert_ne!(sum, sum2);
    }

    #[test]
    fn test_hash_consing() {
        let mut g = AEGraph::new();

        let x = g.add(Node::with_data("const", vec![], 5));
        let y = g.add(Node::with_data("const", vec![], 5));

        // Same constant should give same ID
        assert_eq!(x, y);

        let a = g.add(Node::new("var", vec![]));
        let sum1 = g.add(Node::new("add", vec![a, x]));
        let sum2 = g.add(Node::new("add", vec![a, x]));

        // Same expression should give same ID
        assert_eq!(sum1, sum2);
    }

    #[test]
    fn test_union() {
        let mut g = AEGraph::new();

        let x = g.add(Node::new("var", vec![]));
        let y = g.add(Node::new("var", vec![]));

        // Initially different
        assert_ne!(g.find(x), g.find(y));

        // Union them
        g.union(x, y);

        // Now they should have the same canonical ID
        assert_eq!(g.find(x), g.find(y));
    }
}
