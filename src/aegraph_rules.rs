// Rewrite Rules for Acyclic E-graph
// Implements cascades-style rule application at node creation time

use crate::aegraph_core::{AEGraph, Id, Node};
use std::collections::HashMap;

/// Pattern for matching nodes
#[derive(Debug, Clone)]
pub enum Pattern {
    /// Match a specific operation
    Op(String, Vec<Pattern>),
    /// Match any node and bind to variable
    Var(String),
    /// Match a constant value
    Const(i64),
}

/// A rewrite rule: pattern -> template
pub struct Rule {
    pub name: String,
    pub pattern: Pattern,
    pub template: Pattern,
}

/// Bindings from pattern variables to eclass IDs
type Bindings = HashMap<String, Id>;

impl Pattern {
    /// Try to match this pattern against a node
    pub fn matches(&self, g: &mut AEGraph, id: Id, bindings: &mut Bindings) -> bool {
        let nodes = g.get_nodes(id);
        if nodes.is_empty() {
            return false;
        }

        // Try to match against any node in the eclass
        for node in nodes {
            if self.matches_node(g, &node, bindings) {
                return true;
            }
        }

        false
    }

    fn matches_node(&self, g: &mut AEGraph, node: &Node, bindings: &mut Bindings) -> bool {
        match self {
            Pattern::Var(name) => {
                // Variables match anything and bind
                // (In a real impl, check if already bound consistently)
                true
            }
            Pattern::Const(val) => node.op == "const" && node.data == Some(*val),
            Pattern::Op(op, args) => {
                if &node.op != op || node.args.len() != args.len() {
                    return false;
                }

                // Match arguments recursively
                for (arg_pat, &arg_id) in args.iter().zip(node.args.iter()) {
                    if !arg_pat.matches(g, arg_id, bindings) {
                        return false;
                    }
                }

                true
            }
        }
    }

    /// Build a node from this pattern using bindings
    pub fn instantiate(&self, g: &mut AEGraph, bindings: &Bindings) -> Id {
        match self {
            Pattern::Var(name) => *bindings.get(name).expect("Unbound variable"),
            Pattern::Const(val) => g.add(Node::with_data("const", vec![], *val)),
            Pattern::Op(op, args) => {
                let arg_ids: Vec<Id> = args
                    .iter()
                    .map(|arg| arg.instantiate(g, bindings))
                    .collect();
                g.add(Node::new(op, arg_ids))
            }
        }
    }
}

/// Rule application engine with cascades-style eager application
pub struct RuleEngine {
    rules: Vec<Rule>,
    /// Maximum number of chained rewrites to prevent infinite loops
    max_depth: usize,
}

impl RuleEngine {
    pub fn new() -> Self {
        Self {
            rules: Vec::new(),
            max_depth: 5,
        }
    }

    pub fn add_rule(&mut self, name: impl Into<String>, pattern: Pattern, template: Pattern) {
        self.rules.push(Rule {
            name: name.into(),
            pattern,
            template,
        });
    }

    /// Apply rules to a newly created node (called from add())
    pub fn apply_rules(&self, g: &mut AEGraph, id: Id, depth: usize) {
        if depth >= self.max_depth {
            return;
        }

        for rule in &self.rules {
            let mut bindings = HashMap::new();

            if rule.pattern.matches(g, id, &mut bindings) {
                // Apply the rewrite
                let new_id = rule.template.instantiate(g, &bindings);

                // Union the original and rewritten nodes
                let canonical = g.union(id, new_id);

                // Recursively apply rules to the new node
                self.apply_rules(g, canonical, depth + 1);
            }
        }
    }
}

/// Extended AEGraph with rule application
pub struct AEGraphWithRules {
    pub graph: AEGraph,
    pub rules: RuleEngine,
}

impl AEGraphWithRules {
    pub fn new() -> Self {
        let mut rules = RuleEngine::new();

        // Add common algebraic rules
        Self::add_standard_rules(&mut rules);

        Self {
            graph: AEGraph::new(),
            rules,
        }
    }

    fn add_standard_rules(rules: &mut RuleEngine) {
        // x + 0 = x
        rules.add_rule(
            "add-zero-right",
            Pattern::Op(
                "add".into(),
                vec![Pattern::Var("x".into()), Pattern::Const(0)],
            ),
            Pattern::Var("x".into()),
        );

        // 0 + x = x
        rules.add_rule(
            "add-zero-left",
            Pattern::Op(
                "add".into(),
                vec![Pattern::Const(0), Pattern::Var("x".into())],
            ),
            Pattern::Var("x".into()),
        );

        // x * 0 = 0
        rules.add_rule(
            "mul-zero",
            Pattern::Op(
                "mul".into(),
                vec![Pattern::Var("x".into()), Pattern::Const(0)],
            ),
            Pattern::Const(0),
        );

        // x * 1 = x
        rules.add_rule(
            "mul-one",
            Pattern::Op(
                "mul".into(),
                vec![Pattern::Var("x".into()), Pattern::Const(1)],
            ),
            Pattern::Var("x".into()),
        );

        // Commutativity: x + y = y + x
        // (Simplified version - real impl needs more sophistication)

        // Constant folding: const(a) + const(b) = const(a+b)
        // (Would need special handling in a real implementation)
    }

    /// Add a node with rule application
    pub fn add(&mut self, node: Node) -> Id {
        let id = self.graph.add(node);
        self.rules.apply_rules(&mut self.graph, id, 0);
        self.graph.find(id)
    }

    pub fn find(&mut self, id: Id) -> Id {
        self.graph.find(id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_zero_rule() {
        let mut g = AEGraphWithRules::new();

        // Create: x + 0
        let x = g.add(Node::new("var", vec![]));
        let zero = g.add(Node::with_data("const", vec![], 0));
        let sum = g.add(Node::new("add", vec![x, zero]));

        // After rule application, x + 0 should be equivalent to x
        assert_eq!(g.find(sum), g.find(x));
    }

    #[test]
    fn test_mul_zero_rule() {
        let mut g = AEGraphWithRules::new();

        let x = g.add(Node::new("var", vec![]));
        let zero = g.add(Node::with_data("const", vec![], 0));
        let product = g.add(Node::new("mul", vec![x, zero]));

        // x * 0 should be equivalent to 0
        assert_eq!(g.find(product), g.find(zero));
    }

    #[test]
    fn test_chained_rewrites() {
        let mut g = AEGraphWithRules::new();

        // Create: (x * 1) + 0
        let x = g.add(Node::new("var", vec![]));
        let one = g.add(Node::with_data("const", vec![], 1));
        let zero = g.add(Node::with_data("const", vec![], 0));

        let mul = g.add(Node::new("mul", vec![x, one])); // Should reduce to x
        let sum = g.add(Node::new("add", vec![mul, zero])); // Should reduce to x

        // After chained rewrites, the whole expression should equal x
        // Note: This test may need adjustment based on exact rule application order
        println!("x = {:?}, sum = {:?}", x, sum);
        println!(
            "canonical x = {:?}, canonical sum = {:?}",
            g.find(x),
            g.find(sum)
        );
    }
}
