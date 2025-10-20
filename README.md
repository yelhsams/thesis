# Acyclic E-graph (Aegraph) Implementation

A greenfield implementation of acyclic e-graphs based on Cranelift's design.

## What is an Acyclic E-graph?

An **acyclic e-graph (aegraph)** is an innovation developed for the Cranelift compiler that provides efficient program optimization through equivalence reasoning without the complexity of cycles.

### Key Properties

1. **Immutable E-classes**: Each eclass ID refers to a snapshot that never changes
2. **Append-only Table**: The e-graph is built by only adding entries to the end
3. **Acyclic References**: Nodes can only refer to earlier eclass IDs
4. **Union-Find Canonicalization**: Partial canonicalization using union-find for efficiency
5. **Eager Rewrite Rules**: Rules applied once at node creation (cascades-style)

## Architecture

```
┌─────────────────────┐
│  Entry Table        │ ← Append-only log of nodes and unions
├─────────────────────┤
│  Union-Find         │ ← Tracks canonical representatives
├─────────────────────┤
│  Hash-Cons Map      │ ← Deduplicates identical nodes
├─────────────────────┤
│  Rule Engine        │ ← Applies rewrites eagerly
└─────────────────────┘
```

## Core Components

### 1. `aegraph_core.rs` - Core Data Structures

**`Id`**: Represents an eclass (equivalence class) identifier
- Each ID is a snapshot of an eclass at a point in time
- IDs never change meaning, even as eclasses grow

**`Node`**: Represents an operation with arguments
```rust
Node {
    op: "add",           // Operation kind
    args: [Id(1), Id(2)], // Arguments as eclass IDs
    data: Some(42),      // Optional constant data
}
```

**`EClassEntry`**: Two types of entries
- `Node(node)`: Adds a new expression to the eclass
- `Union(a, b)`: Merges two eclasses

**`UnionFind`**: Tracks canonical representatives
- `find(id)`: Get canonical ID with path compression
- `union(a, b)`: Merge two eclasses, return canonical ID

**`AEGraph`**: The main graph structure
- `add(node)`: Add a node with hash-consing
- `union(a, b)`: Merge eclasses
- `find(id)`: Get canonical ID

### 2. `aegraph_rules.rs` - Rewrite Rules

**`Pattern`**: Matches expressions in the e-graph
- `Op(name, args)`: Match specific operations
- `Var(name)`: Match anything and bind variable
- `Const(val)`: Match constant values

**`RuleEngine`**: Applies rewrite rules
- Rules applied eagerly when nodes are created
- Maximum depth prevents infinite rule chains
- Similar to database "cascades" optimizer

**Example Rules**:
```rust
// x + 0 = x
add_rule("add-zero",
    Pattern::Op("add", [Var("x"), Const(0)]),
    Pattern::Var("x"))

// x * 1 = x
add_rule("mul-one",
    Pattern::Op("mul", [Var("x"), Const(1)]),
    Pattern::Var("x"))
```

### 3. `aegraph_extract.rs` - Term Extraction

**`CostModel`**: Assigns costs to nodes
- Used to select the "best" expression from each eclass
- Example: count operations, minimize memory, etc.

**`Extractor`**: Finds minimum-cost terms
- Recursively evaluates cost of each node
- Caches results for efficiency
- Produces optimized output

**`Term`**: Final extracted expression tree
- Can be pretty-printed
- Can be evaluated for testing

## How It Works

### 1. Building the E-graph

```rust
let mut g = AEGraph::new();

// Add nodes
let x = g.add(Node::new("var", vec![]));
let one = g.add(Node::with_data("const", vec![], 1));
let expr = g.add(Node::new("mul", vec![x, one]));
```

**What happens:**
- Each `add()` call canonicalizes arguments
- Hash-consing checks if node already exists
- If new, creates new eclass entry
- Returns eclass ID

### 2. Applying Rewrite Rules

```rust
let mut g = AEGraphWithRules::new();
let x = g.add(Node::new("var", vec![]));
let one = g.add(Node::with_data("const", vec![], 1));
let expr = g.add(Node::new("mul", vec![x, one]));
// Rule "x * 1 = x" applied automatically
assert_eq!(g.find(expr), g.find(x));
```

**What happens:**
- When `expr` is created, rules are checked
- Pattern matches: `mul(x, 1)`
- Rewrite creates: `x`
- Union: `expr ≡ x`

### 3. Extracting Results

```rust
let mut extractor = Extractor::new(&mut g, OpCountCost);
let term = extractor.build_term(expr);
println!("{}", term.to_string()); // Prints: "x"
```

**What happens:**
- Extractor evaluates all nodes in each eclass
- Selects minimum-cost node
- Recursively builds term tree
- Returns optimized expression

## Key Differences from Traditional E-graphs

| Traditional E-graph | Acyclic E-graph (Aegraph) |
|---------------------|---------------------------|
| Mutable eclasses | Immutable snapshots |
| Cyclic structure | Acyclic by construction |
| Batch rule application | Eager rule application |
| Fixpoint iteration | Single-pass optimization |
| Rebuild/merge phases | Append-only growth |

## Advantages

1. **No Fixpoint Loop**: Rules applied once at creation
2. **Simpler Elaboration**: No cycle breaking needed
3. **Predictable Performance**: Linear passes over code
4. **Memory Efficient**: No redundant merge operations
5. **Easier Reasoning**: Immutable snapshots simplify logic

## Use Cases

- **Compiler Optimizations**: Middle-end IR optimization
- **Query Optimization**: Database query planning
- **Term Rewriting**: Symbolic mathematics
- **Program Synthesis**: Finding optimal implementations

## Integration with Control Flow

Cranelift integrates aegraphs with control flow graphs (CFGs):

1. **Skeleton**: Side-effecting instructions form a skeleton
2. **Pure Operators**: Float freely in the e-graph
3. **Scoped Elaboration**: Extract back to CFG with scheduling

This allows:
- GVN (Global Value Numbering) for free
- LICM (Loop-Invariant Code Motion) during elaboration
- Rematerialization based on register pressure

## Next Steps

To extend this implementation:

1. **Add More Rules**: Commutativity, associativity, distributivity
2. **Pattern Matching**: More sophisticated pattern language
3. **Cost Models**: Different cost functions for different goals
4. **Control Flow**: Integrate with basic blocks
5. **Verification**: Connect with theorem provers (SMT solvers)
6. **Analysis**: Add constant propagation, range analysis

## References

- [Cranelift E-graph RFC](https://github.com/bytecodealliance/rfcs/blob/main/accepted/cranelift-egraph.md)
- [Cranelift E-graph Implementation](https://github.com/bytecodealliance/wasmtime/tree/main/cranelift/codegen/src/egraph)
- [PLDI 2023 Talk: ægraphs](https://pldi23.sigplan.org/details/egraphs-2023-papers/2/-graphs-Acyclic-E-graphs-for-Efficient-Optimization-in-a-Production-Compiler)
- [egg: E-graphs Good](https://egraphs-good.github.io/)
- [Cascades Framework](https://15721.courses.cs.cmu.edu/spring2018/papers/15-optimizer1/graefe-ieee1995.pdf)

## Testing

Run the tests:
```bash
cargo test
```

Each module has comprehensive tests demonstrating:
- Basic operations
- Hash-consing
- Union operations
- Rule application
- Cost-based extraction
- End-to-end optimization

## Performance Considerations

Based on Cranelift's experience:

- **Memory**: Use arena allocation for node storage
- **Deduplication**: Custom hash maps with context
- **Entity-Component Style**: Store data in flat arrays
- **Pooled Storage**: Reuse storage for lists
- **Path Compression**: Essential for union-find performance

## License

This implementation is provided as educational material for understanding acyclic e-graphs.
