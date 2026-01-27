//! IR Types with CLIF Display Support
//!
//! This is an extended version of types.rs that includes CLIF pretty-printing methods.

use std::collections::HashMap;
use std::fmt;

/// Basic block
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BlockId(pub u32);

/// Instruction
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
    AddImm,
    MulImm,
    ShlImm,
    Shl,
    Ushr,
    Sshr,
    Bnot,
    Ineg,
    Eq,
    Ne,
    Slt,
    Sle,
    Sgt,
    Sge,
    Ult,
    Ule,
    Ugt,
    Uge,
    Uextend,
    Sextend,

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
                | Opcode::AddImm
                | Opcode::MulImm
                | Opcode::ShlImm
                | Opcode::Shl
                | Opcode::Ushr
                | Opcode::Sshr
                | Opcode::Bnot
                | Opcode::Ineg
                | Opcode::Eq
                | Opcode::Ne
                | Opcode::Slt
                | Opcode::Sle
                | Opcode::Sgt
                | Opcode::Sge
                | Opcode::Ult
                | Opcode::Ule
                | Opcode::Ugt
                | Opcode::Uge
                | Opcode::Uextend
                | Opcode::Sextend
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
        matches!(self, Opcode::Trap | Opcode::Div)
    }
}

/// Branch target information for control flow instructions
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BranchInfo {
    /// Unconditional jump: target block
    Jump(BlockId),
    /// Conditional branch: (then_block, then_args_count, else_block, else_args_count)
    /// Note: first arg in instruction is the condition
    Conditional(BlockId, usize, BlockId, usize),
}

/// Instruction
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Instruction {
    pub opcode: Opcode,
    pub args: Vec<ValueId>,
    pub ty: Type,
    pub immediate: Option<i64>,          // For constants
    pub branch_info: Option<BranchInfo>, // For branch/brif instructions
}

impl Instruction {
    pub fn new(opcode: Opcode, args: Vec<ValueId>, ty: Type) -> Self {
        Self {
            opcode,
            args,
            ty,
            immediate: None,
            branch_info: None,
        }
    }

    pub fn with_imm(opcode: Opcode, args: Vec<ValueId>, ty: Type, imm: i64) -> Self {
        Self {
            opcode,
            args,
            ty,
            immediate: Some(imm),
            branch_info: None,
        }
    }

    pub fn with_branch(
        opcode: Opcode,
        args: Vec<ValueId>,
        ty: Type,
        branch_info: BranchInfo,
    ) -> Self {
        Self {
            opcode,
            args,
            ty,
            immediate: None,
            branch_info: Some(branch_info),
        }
    }

    pub fn can_merge_with(&self, other: &Instruction, _dfg: &DataFlowGraph) -> bool {
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
    pub fn first_result(&self, inst: InstId) -> ValueId {
        self.inst_results[&inst][0]
    }

    /// Check if two instructions are mergeable
    pub fn instructions_mergeable(&self, inst1: InstId, inst2: InstId) -> bool {
        let i1 = &self.insts[&inst1];
        let i2 = &self.insts[&inst2];

        i1.can_merge_with(i2, self)
    }

    /// Display an instruction in CLIF format
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

        // Type suffix (except for control flow)
        if !matches!(
            inst.opcode,
            Opcode::Branch | Opcode::CondBranch | Opcode::Return
        ) {
            s.push_str(&format!(".{}", inst.ty));
        }

        // For constants, show immediate value
        if inst.opcode == Opcode::Const {
            if let Some(imm) = inst.immediate {
                s.push_str(&format!(" {}", imm));
            }
        } else if inst.opcode == Opcode::Branch {
            // For jump instructions, show target block with arguments
            if let Some(BranchInfo::Jump(block_id)) = &inst.branch_info {
                s.push_str(&format!(" block{}", block_id.0));
                if !inst.args.is_empty() {
                    s.push('(');
                    for (i, &arg) in inst.args.iter().enumerate() {
                        if i > 0 {
                            s.push_str(", ");
                        }
                        s.push_str(&format!("{}", arg));
                    }
                    s.push(')');
                }
            }
        } else if inst.opcode == Opcode::CondBranch {
            // For conditional branches, show condition, then block, and else block
            if let Some(BranchInfo::Conditional(then_block, then_count, else_block, else_count)) =
                &inst.branch_info
            {
                // First arg is the condition
                if !inst.args.is_empty() {
                    s.push_str(&format!(" {}", inst.args[0]));
                    s.push_str(", ");

                    // Then block
                    s.push_str(&format!("block{}", then_block.0));
                    if *then_count > 0 {
                        s.push('(');
                        for i in 0..*then_count {
                            if i > 0 {
                                s.push_str(", ");
                            }
                            s.push_str(&format!("{}", inst.args[1 + i]));
                        }
                        s.push(')');
                    }
                    s.push_str(", ");

                    // Else block
                    s.push_str(&format!("block{}", else_block.0));
                    if *else_count > 0 {
                        s.push('(');
                        for i in 0..*else_count {
                            if i > 0 {
                                s.push_str(", ");
                            }
                            s.push_str(&format!("{}", inst.args[1 + then_count + i]));
                        }
                        s.push(')');
                    }
                }
            }
        } else if !inst.args.is_empty() {
            // For other instructions, show arguments
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

    /// Display the entire function in CLIF format
    pub fn display(
        &self,
        dfg: &DataFlowGraph,
        func_name: &str,
        sig_params: &[Type],
        sig_return: Option<Type>,
    ) -> String {
        let mut s = String::new();

        // Function signature
        s.push_str("function %");
        s.push_str(func_name);
        s.push('(');
        for (i, ty) in sig_params.iter().enumerate() {
            if i > 0 {
                s.push_str(", ");
            }
            s.push_str(&format!("{}", ty));
        }
        s.push(')');

        if let Some(ret_ty) = sig_return {
            s.push_str(" -> ");
            s.push_str(&format!("{}", ret_ty));
        }

        s.push_str(" {\n");

        // Blocks
        for (idx, &block_id) in self.blocks.iter().enumerate() {
            let block = &self.block_data[&block_id];

            // Blank line before block (except first)
            if idx > 0 {
                s.push('\n');
            }

            // Block header
            s.push_str(&format!("{}(", block_id));
            for (i, &param) in block.params.iter().enumerate() {
                if i > 0 {
                    s.push_str(", ");
                }
                s.push_str(&format!("{}: {}", param, dfg.value_type(param)));
            }
            s.push_str("):\n");

            // Instructions
            for &inst_id in &block.insts {
                s.push_str(&format!("    {}\n", dfg.display_inst(inst_id)));
            }
        }

        s.push_str("}\n");
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
        println!("\n=== Egraph Pass Statistics ===");
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

// Display implementations
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
            Opcode::And => "band",
            Opcode::Or => "bor",
            Opcode::Xor => "bxor",
            Opcode::Const => "iconst",
            Opcode::AddImm => "iadd_imm",
            Opcode::MulImm => "imul_imm",
            Opcode::ShlImm => "ishl_imm",
            Opcode::Shl => "ishl",
            Opcode::Ushr => "ushr",
            Opcode::Sshr => "sshr",
            Opcode::Bnot => "bnot",
            Opcode::Ineg => "ineg",
            Opcode::Eq => "icmp.eq",
            Opcode::Ne => "icmp.ne",
            Opcode::Slt => "icmp.slt",
            Opcode::Sle => "icmp.sle",
            Opcode::Sgt => "icmp.sgt",
            Opcode::Sge => "icmp.sge",
            Opcode::Ult => "icmp.ult",
            Opcode::Ule => "icmp.ule",
            Opcode::Ugt => "icmp.ugt",
            Opcode::Uge => "icmp.uge",
            Opcode::Uextend => "uextend",
            Opcode::Sextend => "sextend",
            Opcode::Load => "load",
            Opcode::Store => "store",
            Opcode::Call => "call",
            Opcode::Branch => "jump",
            Opcode::CondBranch => "brif",
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
