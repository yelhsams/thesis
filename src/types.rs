use std::collections::HashMap;
use std::fmt;

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
    I8,
    I16,
    I32,
    I64,
}

/// Mini-lang to test the egraph pass
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Opcode {
    // Pure ops
    Add,
    Sub,
    Mul,
    And,
    Or,
    Xor,
    Const,

    // Side-effect ops
    Div,
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
        // TODO: must make sure that the divisor is not zero
        // TODO: make this only merge on traps with same condition
        matches!(self, Opcode::Trap | Opcode::Div)
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

    pub fn can_merge_with(&self, other: &Instruction, dfg: &DataFlowGraph) -> bool {
        if self.opcode != other.opcode || self.ty != other.ty {
            return false;
        }
        match self.opcode {
            Opcode::Trap => self.args.iter().zip(other.args.iter()).all(|(a, b)| a == b),
            Opcode::Div => self.args[0] == other.args[0] && self.args[1] == other.args[1],
            _ => false,
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

    /// Check if two instructions are mergeable
    pub fn instructions_mergeable(&self, inst1: InstId, inst2: InstId) -> bool {
        let i1 = &self.insts[&inst1];
        let i2 = &self.insts[&inst2];

        i1.can_merge_with(i2, self)
    }

    /// Display an instruction in a human-readable format
    pub fn display_inst(&self, inst_id: InstId) -> String {
        let inst = &self.insts[&inst_id];
        let results = self.inst_results(inst_id);

        let mut s = String::new();

        // Results (if any)
        if !results.is_empty() {
            for (i, &result) in results.iter().enumerate() {
                if i > 0 {
                    s.push_str(", ");
                }
                s.push_str(&format!("{}", result));
            }
            s.push_str(" = ");
        }

        // Opcode
        s.push_str(&format!("{}", inst.opcode));

        // Type (for most operations)
        if !matches!(
            inst.opcode,
            Opcode::Branch | Opcode::CondBranch | Opcode::Return
        ) {
            s.push_str(&format!(".{}", inst.ty));
        }

        // Immediate (for constants)
        if let Some(imm) = inst.immediate {
            s.push_str(&format!(" {}", imm));
        }

        // Arguments (if any and not a constant)
        if !inst.args.is_empty() && inst.opcode != Opcode::Const {
            s.push(' ');
            for (i, &arg) in inst.args.iter().enumerate() {
                if i > 0 {
                    s.push_str(", ");
                }
                s.push_str(&format!("{}", arg));
            }
        }

        s
    }

    /// Display a value definition
    pub fn display_value_def(&self, value: ValueId) -> String {
        match self.value_def(value) {
            ValueDef::Inst(inst_id) => {
                format!("{} = {}", value, self.display_inst(inst_id))
            }
            ValueDef::BlockParam(block, index) => {
                format!("{} = param {} of {}", value, index, block)
            }
            ValueDef::Union(left, right) => {
                format!("{} = union({}, {})", value, left, right)
            }
        }
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

    /// Display the entire CFG in a human-readable format
    pub fn display(&self, dfg: &DataFlowGraph) -> String {
        let mut s = String::new();

        for &block_id in &self.blocks {
            let block = &self.block_data[&block_id];

            // Block header
            s.push_str(&format!("{}:", block_id));

            // Block parameters
            if !block.params.is_empty() {
                s.push('(');
                for (i, &param) in block.params.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&format!("{}: {}", param, dfg.value_type(param)));
                }
                s.push(')');
            }
            s.push('\n');

            // Instructions
            for &inst_id in &block.insts {
                // Skip terminator, we'll show it separately
                if Some(inst_id) == block.terminator {
                    continue;
                }
                s.push_str(&format!("    {}\n", dfg.display_inst(inst_id)));
            }

            // Terminator
            if let Some(term_id) = block.terminator {
                s.push_str(&format!("    {}\n", dfg.display_inst(term_id)));
            }

            s.push('\n');
        }

        s
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

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "block{}", self.0)
    }
}

impl fmt::Display for InstId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "inst{}", self.0)
    }
}

impl fmt::Display for ValueId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Opcode::Add => "iadd",
            Opcode::Sub => "isub",
            Opcode::Mul => "imul",
            Opcode::Div => "idiv",
            Opcode::And => "and",
            Opcode::Or => "or",
            Opcode::Xor => "xor",
            Opcode::Const => "iconst",
            Opcode::Load => "load",
            Opcode::Store => "store",
            Opcode::Call => "call",
            Opcode::Branch => "jump",
            Opcode::CondBranch => "br_if",
            Opcode::Return => "return",
            Opcode::Trap => "trap",
        };
        write!(f, "{}", s)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Type::I8 => "i8",
            Type::I16 => "i16",
            Type::I32 => "i32",
            Type::I64 => "i64",
        };
        write!(f, "{}", s)
    }
}
