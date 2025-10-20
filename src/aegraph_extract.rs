// Extraction (Elaboration) from Acyclic E-graph
// Selects the best term from each eclass to produce optimized output

use crate::aegraph_core::{AEGraph, Id, Node};
use std::collections::HashMap;

/// Cost model for selecting the best term from an eclass
pub trait CostModel {
    fn cost(&self, node: &Node, arg_costs: &[Cost]) -> Cost;
}

/// Represents the cost of a node
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Cost(pub u32);

impl Cost {
    pub const INFINITY: Cost = Cost(u32::MAX);
}

/// Simple cost model: count the number of operations
pub struct OpCountCost;

impl CostModel for OpCountCost {
    fn cost(&self, node: &Node, arg_costs: &[Cost]) -> Cost {
        let base_cost = match node.op.as_str() {
            "const" | "var" => 0, // Free
            "add" | "sub" => 1,
            "mul" => 2,
            "div" => 5,
            _ => 1,
        };

        let total: u32 = arg_costs.iter().map(|c| c.0).sum();

        Cost(base_cost + total)
    }
}

/// Extractor that finds the minimum-cost term in each eclass
pub struct Extractor<'a, C: CostModel> {
    graph: &'a mut AEGraph,
    cost_model: C,
    /// Cache of best costs for each eclass
    best_cost: HashMap<Id, Cost>,
    /// Cache of best node for each eclass
    best_node: HashMap<Id, Node>,
}

impl<'a, C: CostModel> Extractor<'a, C> {
    pub fn new(graph: &'a mut AEGraph, cost_model: C) -> Self {
        Self {
            graph,
            cost_model,
            best_cost: HashMap::new(),
            best_node: HashMap::new(),
        }
    }

    /// Extract the minimum-cost term from an eclass
    pub fn extract(&mut self, id: Id) -> (Cost, Node) {
        let canonical = self.graph.find(id);

        // Check cache
        if let Some(&cost) = self.best_cost.get(&canonical) {
            let node = self.best_node.get(&canonical).unwrap().clone();
            return (cost, node);
        }

        // Find all nodes in the eclass
        let nodes = self.graph.get_nodes(canonical);

        if nodes.is_empty() {
            return (Cost::INFINITY, Node::new("error", vec![]));
        }

        let mut best_cost = Cost::INFINITY;
        let mut best_node = nodes[0].clone();

        // Evaluate cost of each node
        for node in nodes {
            let cost = self.evaluate_node(&node);

            if cost < best_cost {
                best_cost = cost;
                best_node = node;
            }
        }

        // Cache result
        self.best_cost.insert(canonical, best_cost);
        self.best_node.insert(canonical, best_node.clone());

        (best_cost, best_node)
    }

    /// Recursively evaluate the cost of a node
    fn evaluate_node(&mut self, node: &Node) -> Cost {
        // Recursively extract arguments
        let mut arg_costs = Vec::new();

        for &arg_id in &node.args {
            let (cost, _) = self.extract(arg_id);
            arg_costs.push(cost);
        }

        // Compute cost using model
        self.cost_model.cost(node, &arg_costs)
    }

    /// Extract and build a complete expression tree
    pub fn build_term(&mut self, id: Id) -> Term {
        let (_, node) = self.extract(id);

        let args: Vec<Term> = node
            .args
            .iter()
            .map(|&arg_id| self.build_term(arg_id))
            .collect();

        Term {
            op: node.op.clone(),
            args,
            data: node.data,
        }
    }
}

/// A term extracted from the e-graph
#[derive(Debug, Clone)]
pub struct Term {
    pub op: String,
    pub args: Vec<Term>,
    pub data: Option<i64>,
}

impl Term {
    /// Pretty-print the term
    pub fn to_string(&self) -> String {
        match self.op.as_str() {
            "const" => format!("{}", self.data.unwrap_or(0)),
            "var" => "x".to_string(),
            "add" if self.args.len() == 2 => {
                format!(
                    "({} + {})",
                    self.args[0].to_string(),
                    self.args[1].to_string()
                )
            }
            "sub" if self.args.len() == 2 => {
                format!(
                    "({} - {})",
                    self.args[0].to_string(),
                    self.args[1].to_string()
                )
            }
            "mul" if self.args.len() == 2 => {
                format!(
                    "({} * {})",
                    self.args[0].to_string(),
                    self.args[1].to_string()
                )
            }
            "div" if self.args.len() == 2 => {
                format!(
                    "({} / {})",
                    self.args[0].to_string(),
                    self.args[1].to_string()
                )
            }
            _ => {
                let args_str: Vec<String> = self.args.iter().map(|arg| arg.to_string()).collect();
                format!("{}({})", self.op, args_str.join(", "))
            }
        }
    }

    /// Evaluate the term (for testing)
    pub fn eval(&self, var_value: i64) -> i64 {
        match self.op.as_str() {
            "const" => self.data.unwrap_or(0),
            "var" => var_value,
            "add" => self.args[0].eval(var_value) + self.args[1].eval(var_value),
            "sub" => self.args[0].eval(var_value) - self.args[1].eval(var_value),
            "mul" => self.args[0].eval(var_value) * self.args[1].eval(var_value),
            "div" => self.args[0].eval(var_value) / self.args[1].eval(var_value),
            _ => 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_extraction() {
        let mut g = AEGraph::new();

        // Create: (x + 0) + (y * 1)
        let x = g.add(Node::new("var", vec![]));
        let y = g.add(Node::new("var", vec![]));
        let zero = g.add(Node::with_data("const", vec![], 0));
        let one = g.add(Node::with_data("const", vec![], 1));

        let x_plus_0 = g.add(Node::new("add", vec![x, zero]));
        let y_times_1 = g.add(Node::new("mul", vec![y, one]));
        let result = g.add(Node::new("add", vec![x_plus_0, y_times_1]));

        // Extract
        let mut extractor = Extractor::new(&mut g, OpCountCost);
        let term = extractor.build_term(result);

        println!("Extracted term: {}", term.to_string());
    }

    #[test]
    fn test_cost_selection() {
        let mut g = AEGraph::new();

        // Create two equivalent expressions with different costs
        let x = g.add(Node::new("var", vec![]));

        // Expression 1: x (cost 0)
        let expr1 = x;

        // Expression 2: x * 1 (cost 2)
        let one = g.add(Node::with_data("const", vec![], 1));
        let expr2 = g.add(Node::new("mul", vec![x, one]));

        // Union them
        g.union(expr1, expr2);

        // Extract - should prefer expr1 (lower cost)
        let mut extractor = Extractor::new(&mut g, OpCountCost);
        let (cost, node) = extractor.extract(expr1);

        println!("Best cost: {:?}", cost);
        println!("Best node: {:?}", node);

        // Should select the "var" node (cost 0) over "mul" (cost 2)
        assert_eq!(cost, Cost(0));
        assert_eq!(node.op, "var");
    }

    #[test]
    fn test_full_optimization() {
        use crate::aegraph_rules::AEGraphWithRules;

        let mut g = AEGraphWithRules::new();

        // Create: (x + 0) * 1 + (y * 0)
        let x = g.add(Node::new("var", vec![]));
        let y = g.add(Node::new("var", vec![]));
        let zero = g.add(Node::with_data("const", vec![], 0));
        let one = g.add(Node::with_data("const", vec![], 1));

        let x_plus_0 = g.add(Node::new("add", vec![x, zero]));
        let result1 = g.add(Node::new("mul", vec![x_plus_0, one]));
        let y_times_0 = g.add(Node::new("mul", vec![y, zero]));
        let final_result = g.add(Node::new("add", vec![result1, y_times_0]));

        // Extract the optimized term
        let mut extractor = Extractor::new(&mut g.graph, OpCountCost);
        let term = extractor.build_term(final_result);

        println!("Original: ((x + 0) * 1) + (y * 0)");
        println!("Optimized: {}", term.to_string());

        // Should optimize to just "x" since:
        // - x + 0 = x
        // - x * 1 = x
        // - y * 0 = 0
        // - x + 0 = x
    }
}
