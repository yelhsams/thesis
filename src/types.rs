use std::collections::HashMap;

/// Basic block
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BlockId(pub u32);

/// Instruction
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstId(pub u32);

/// Value (SSA value)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ValueId(pub u32);

impl ValueId {
    pub const RESERVED: ValueId = ValueId(u32::MAX);

    pub fn is_reserved(self) -> bool {
        self == Self::RESERVED
    }
}

/// Types of values
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
    Ptr,
}

/// Mini-lang to test the egraph pass
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Opcode {
    // Pure ops
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Xor,
    Const,

    // Side-effect ops
    Load,
    Store,
    Call,
    Branch,
    CondBranch,
    Return,
    Trap,
}

impl Opcode {
    /// Returns true if this is a pure operation (no side effects)
    pub fn is_pure(self) -> bool {
        matches!(
            self,
            Opcode::Add
                | Opcode::Sub
                | Opcode::Mul
                | Opcode::Div
                | Opcode::And
                | Opcode::Or
                | Opcode::Xor
                | Opcode::Const
        )
    }

    /// Returns true if this ends a block
    pub fn is_terminator(self) -> bool {
        matches!(
            self,
            Opcode::Branch | Opcode::CondBranch | Opcode::Return | Opcode::Trap
        )
    }

    /// Returns true if this operation can be merged (deduplicated) even though
    /// it has side effects
    pub fn is_mergeable(self) -> bool {
        // Some side-effecting ops can still be GVN'd if they're idempotent
        // QUESTION! WHAT IS THE BEHAVIOR IN CRANELIFT?
        matches!(self, Opcode::Load)
    }
}

/// Instruction
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Instruction {
    pub opcode: Opcode,
    pub args: Vec<ValueId>,
    pub ty: Type,
    pub immediate: Option<i64>, // For constants
}

impl Instruction {
    pub fn new(opcode: Opcode, args: Vec<ValueId>, ty: Type) -> Self {
        Self {
            opcode,
            args,
            ty,
            immediate: None,
        }
    }

    pub fn with_imm(opcode: Opcode, args: Vec<ValueId>, ty: Type, imm: i64) -> Self {
        Self {
            opcode,
            args,
            ty,
            immediate: Some(imm),
        }
    }
}

/// Definition of a value - where it comes from
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueDef {
    /// Result of an instruction
    Inst(InstId),
    /// Block parameter (phi node equivalent)
    BlockParam(BlockId, usize),
    /// Union node (part of egraph)
    Union(ValueId, ValueId),
}

/// A basic block in the CFG
#[derive(Debug, Clone)]
pub struct Block {
    pub id: BlockId,
    pub params: Vec<ValueId>,
    pub insts: Vec<InstId>,
    pub terminator: Option<InstId>,
}

impl Block {
    pub fn new(id: BlockId) -> Self {
        Self {
            id,
            params: Vec::new(),
            insts: Vec::new(),
            terminator: None,
        }
    }
}

/// The data flow graph
#[derive(Debug, Clone)]
pub struct DataFlowGraph {
    /// All instructions
    pub insts: HashMap<InstId, Instruction>,

    /// Map from instruction to its result values
    pub inst_results: HashMap<InstId, Vec<ValueId>>,

    /// Map from value to its definition
    pub value_defs: HashMap<ValueId, ValueDef>,

    /// Map from value to its type
    pub value_types: HashMap<ValueId, Type>,

    /// Next available IDs
    next_inst: u32,
    next_value: u32,
}

impl DataFlowGraph {
    pub fn new() -> Self {
        Self {
            insts: HashMap::new(),
            inst_results: HashMap::new(),
            value_defs: HashMap::new(),
            value_types: HashMap::new(),
            next_inst: 0,
            next_value: 0,
        }
    }

    /// Create a new instruction and return its ID
    pub fn make_inst(&mut self, inst: Instruction) -> InstId {
        let id = InstId(self.next_inst);
        self.next_inst += 1;
        self.insts.insert(id, inst);
        id
    }

    /// Create a result value for an instruction
    pub fn make_inst_result(&mut self, inst_id: InstId, ty: Type) -> ValueId {
        let value_id = ValueId(self.next_value);
        self.next_value += 1;

        self.value_types.insert(value_id, ty);
        self.value_defs.insert(value_id, ValueDef::Inst(inst_id));

        self.inst_results
            .entry(inst_id)
            .or_insert_with(Vec::new)
            .push(value_id);

        value_id
    }

    /// Create a block parameter
    pub fn make_block_param(&mut self, block: BlockId, index: usize, ty: Type) -> ValueId {
        let value_id = ValueId(self.next_value);
        self.next_value += 1;

        self.value_types.insert(value_id, ty);
        self.value_defs
            .insert(value_id, ValueDef::BlockParam(block, index));

        value_id
    }

    /// Create a union node
    pub fn make_union(&mut self, left: ValueId, right: ValueId) -> ValueId {
        let value_id = ValueId(self.next_value);
        self.next_value += 1;

        // Union takes type from left value
        let ty = self.value_types[&left];
        self.value_types.insert(value_id, ty);
        self.value_defs
            .insert(value_id, ValueDef::Union(left, right));

        value_id
    }

    /// Get the definition of a value
    pub fn value_def(&self, value: ValueId) -> ValueDef {
        self.value_defs[&value]
    }

    /// Get the type of a value
    pub fn value_type(&self, value: ValueId) -> Type {
        self.value_types[&value]
    }

    /// Get the results of an instruction
    pub fn inst_results(&self, inst: InstId) -> &[ValueId] {
        self.inst_results
            .get(&inst)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }

    /// Returns the first SSA value produced by the given instruction.
    /// QUESTION: Doesn't handle multiple results.
    pub fn first_result(&self, inst: InstId) -> ValueId {
        self.inst_results[&inst][0]
    }
}

/// Control flow graph
#[derive(Debug, Clone)]
pub struct Layout {
    pub blocks: Vec<BlockId>,
    pub block_data: HashMap<BlockId, Block>,
}

impl Layout {
    pub fn new() -> Self {
        Self {
            blocks: Vec::new(),
            block_data: HashMap::new(),
        }
    }

    pub fn add_block(&mut self, block: Block) {
        self.blocks.push(block.id);
        self.block_data.insert(block.id, block);
    }

    pub fn entry_block(&self) -> Option<BlockId> {
        self.blocks.first().copied()
    }
}

/// Statistics collected during egraph pass
#[derive(Debug, Clone, Default)]
pub struct Stats {
    pub pure_inst: u64,
    pub pure_inst_deduped: u64,
    pub pure_inst_subsume: u64,
    pub pure_inst_insert_new: u64,
    pub skeleton_inst: u64,
    pub skeleton_inst_gvn: u64,
    pub new_inst: u64,
    pub union: u64,
    pub rewrite_rule_invoked: u64,
    pub rewrite_rule_results: u64,
    pub rewrite_depth_limit: u64,
    pub eclass_size_limit: u64,
    pub elaborate_visit_node: u64,
    pub elaborate_memoize_hit: u64,
    pub elaborate_memoize_miss: u64,
}

impl Stats {
    pub fn print_summary(&self) {
        println!("=== Egraph Pass Statistics ===");
        println!("Pure instructions processed: {}", self.pure_inst);
        println!("  - Deduplicated: {}", self.pure_inst_deduped);
        println!("  - New: {}", self.pure_inst_insert_new);
        println!("Skeleton instructions: {}", self.skeleton_inst);
        println!("  - GVN merged: {}", self.skeleton_inst_gvn);
        println!("Union nodes created: {}", self.union);
        println!("Rewrite rules invoked: {}", self.rewrite_rule_invoked);
        println!("Rewrite results: {}", self.rewrite_rule_results);
    }
}
