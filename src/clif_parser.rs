//! CLIF Parser - Parses Cranelift IR text format into our IR data structures

use crate::clif_lexer::*;
use crate::types::*;
use std::collections::HashMap;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,

    // Mapping from value names (v0, v1, etc.) to ValueIds
    value_map: HashMap<String, ValueId>,

    // Mapping from block names (block0, block1, etc.) to BlockIds
    block_map: HashMap<String, BlockId>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            value_map: HashMap::new(),
            block_map: HashMap::new(),
        }
    }

    pub fn parse(&mut self) -> Result<(DataFlowGraph, Layout), String> {
        let mut dfg = DataFlowGraph::new();
        let mut layout = Layout::new();

        // Skip leading newlines
        self.skip_newlines();

        // Parse function signature
        self.parse_function_signature()?;

        // Parse blocks
        while !self.is_at_end() && !self.check(Token::RBrace) {
            self.skip_newlines();
            if self.is_at_end() || self.check(Token::RBrace) {
                break;
            }
            self.parse_block(&mut dfg, &mut layout)?;
        }

        Ok((dfg, layout))
    }

    fn parse_function_signature(&mut self) -> Result<(), String> {
        self.expect_keyword("function")?;

        // Function name
        if let Token::FuncName(_name) = self.peek() {
            self.advance();
        } else {
            return Err(format!("Expected function name, got {:?}", self.peek()));
        }

        // Parameters
        self.expect(Token::LParen)?;
        if !self.check(Token::RParen) {
            self.parse_param_list()?;
        }
        self.expect(Token::RParen)?;

        // Return type (optional)
        if self.check(Token::Arrow) {
            self.advance();
            self.parse_type()?;
        }

        self.expect(Token::LBrace)?;
        self.skip_newlines();

        Ok(())
    }

    fn parse_param_list(&mut self) -> Result<(), String> {
        loop {
            self.parse_type()?;

            if !self.check(Token::Comma) {
                break;
            }
            self.advance();
        }
        Ok(())
    }

    fn parse_block(&mut self, dfg: &mut DataFlowGraph, layout: &mut Layout) -> Result<(), String> {
        // Parse block name
        let block_name = if let Token::BlockName(name) = self.peek() {
            let name = name.clone();
            self.advance();
            name
        } else {
            return Err(format!("Expected block name, got {:?}", self.peek()));
        };

        // Extract block number
        let block_num = block_name
            .trim_start_matches("block")
            .parse::<u32>()
            .map_err(|_| format!("Invalid block name: {}", block_name))?;
        let block_id = BlockId(block_num);

        self.block_map.insert(block_name.clone(), block_id);

        // Parse block parameters
        let mut block_params = Vec::new();
        if self.check(Token::LParen) {
            self.advance();

            if !self.check(Token::RParen) {
                loop {
                    let (param_name, param_type) = self.parse_value_with_type()?;
                    let param_value =
                        dfg.make_block_param(block_id, block_params.len(), param_type);
                    self.value_map.insert(param_name, param_value);
                    block_params.push(param_value);

                    if !self.check(Token::Comma) {
                        break;
                    }
                    self.advance();
                }
            }

            self.expect(Token::RParen)?;
        }

        self.expect(Token::Colon)?;
        self.skip_newlines();

        // Create block
        let mut block = Block::new(block_id);
        block.params = block_params;

        // Parse instructions
        while !self.is_at_end() && !self.check_block_start() && !self.check(Token::RBrace) {
            self.skip_newlines();
            if self.is_at_end() || self.check_block_start() || self.check(Token::RBrace) {
                break;
            }

            let inst_id = self.parse_instruction(dfg, block_id)?;
            block.insts.push(inst_id);

            // Check if this is a terminator
            let inst = &dfg.insts[&inst_id];
            if inst.opcode.is_terminator() {
                block.terminator = Some(inst_id);
                break;
            }

            self.skip_newlines();
        }

        layout.add_block(block);
        Ok(())
    }

    fn parse_instruction(
        &mut self,
        dfg: &mut DataFlowGraph,
        _block_id: BlockId,
    ) -> Result<InstId, String> {
        // Check if this is a value-producing instruction
        let has_result = if let Token::Value(_) = self.peek() {
            if self.peek_ahead(1) == Token::Equals {
                true
            } else {
                false
            }
        } else {
            false
        };

        let result_name = if has_result {
            let name = if let Token::Value(n) = self.peek() {
                n.clone()
            } else {
                unreachable!()
            };
            self.advance(); // consume value name
            self.expect(Token::Equals)?;
            Some(name)
        } else {
            None
        };

        // Parse opcode
        let opcode_str = if let Token::Opcode(op) = self.peek() {
            op.clone()
        } else {
            return Err(format!("Expected opcode, got {:?}", self.peek()));
        };
        self.advance();

        let mut opcode = self.parse_opcode(&opcode_str)?;

        // For icmp, check if there's a comparison condition after a dot
        if opcode_str == "icmp" && self.check(Token::Dot) {
            self.advance();
            if let Token::Opcode(cond) = self.peek() {
                let cond_str = cond.clone();
                self.advance();
                opcode = self.parse_comparison_condition(&cond_str)?;
            } else {
                return Err(format!(
                    "Expected comparison condition after 'icmp.', got {:?}",
                    self.peek()
                ));
            }
        }

        // Parse type suffix (e.g., .i32)
        let ty = if self.check(Token::Dot) {
            self.advance();
            self.parse_type()?
        } else {
            Type::I32 // default
        };

        // Parse arguments
        let (args, immediate, branch_info) = self.parse_instruction_args(dfg, &opcode)?;

        // Create instruction
        let inst = if let Some(branch_info) = branch_info {
            Instruction::with_branch(opcode, args, ty, branch_info)
        } else if let Some(imm) = immediate {
            Instruction::with_imm(opcode, args, ty, imm)
        } else {
            Instruction::new(opcode, args, ty)
        };

        let inst_id = dfg.make_inst(inst);

        // Create result value if needed
        if let Some(name) = result_name {
            let result_value = dfg.make_inst_result(inst_id, ty);
            self.value_map.insert(name, result_value);
        }

        Ok(inst_id)
    }

    fn parse_instruction_args(
        &mut self,
        dfg: &DataFlowGraph,
        opcode: &Opcode,
    ) -> Result<(Vec<ValueId>, Option<i64>, Option<BranchInfo>), String> {
        let mut args = Vec::new();
        let mut immediate = None;
        let mut branch_info = None;

        // Special handling for different instruction types
        match opcode {
            Opcode::Const => {
                // iconst takes just an immediate value
                if let Token::Integer(n) = self.peek() {
                    immediate = Some(n);
                    self.advance();
                }
            }

            Opcode::AddImm | Opcode::MulImm | Opcode::ShlImm => {
                // Immediate variants: value, immediate
                // e.g., iadd_imm v0, 10
                let value = self.parse_value_ref()?;
                args.push(value);

                self.expect(Token::Comma)?;

                if let Token::Integer(n) = self.peek() {
                    immediate = Some(n);
                    self.advance();
                } else {
                    return Err(format!("Expected integer immediate, got {:?}", self.peek()));
                }
            }

            Opcode::Branch => {
                // jump block(args)
                let target_block = self.parse_block_ref()?;

                // Parse jump arguments
                if self.check(Token::LParen) {
                    self.advance();
                    if !self.check(Token::RParen) {
                        loop {
                            let value = self.parse_value_ref()?;
                            args.push(value);

                            if !self.check(Token::Comma) {
                                break;
                            }
                            self.advance();
                        }
                    }
                    self.expect(Token::RParen)?;
                }
                branch_info = Some(BranchInfo::Jump(target_block));
            }

            Opcode::CondBranch => {
                // brif cond, block1(args), block2(args)
                let cond = self.parse_value_ref()?;
                args.push(cond);

                self.expect(Token::Comma)?;

                // Then block
                let then_block = self.parse_block_ref()?;
                let mut then_args_count = 0;
                if self.check(Token::LParen) {
                    self.advance();
                    if !self.check(Token::RParen) {
                        loop {
                            let value = self.parse_value_ref()?;
                            args.push(value);
                            then_args_count += 1;
                            if !self.check(Token::Comma) {
                                break;
                            }
                            self.advance();
                        }
                    }
                    self.expect(Token::RParen)?;
                }

                self.expect(Token::Comma)?;

                // Else block
                let else_block = self.parse_block_ref()?;
                let mut else_args_count = 0;
                if self.check(Token::LParen) {
                    self.advance();
                    if !self.check(Token::RParen) {
                        loop {
                            let value = self.parse_value_ref()?;
                            args.push(value);
                            else_args_count += 1;

                            if !self.check(Token::Comma) {
                                break;
                            }
                            self.advance();
                        }
                    }
                    self.expect(Token::RParen)?;
                }
                branch_info = Some(BranchInfo::Conditional(
                    then_block,
                    then_args_count,
                    else_block,
                    else_args_count,
                ));
            }

            Opcode::Return => {
                // return value (optional)
                if !self.check(Token::Newline) && !self.is_at_end() {
                    let value = self.parse_value_ref()?;
                    args.push(value);
                }
            }

            _ => {
                // Binary/unary operations: parse value arguments
                if !self.check(Token::Newline) && !self.is_at_end() {
                    loop {
                        // Check if we're at a comment
                        if self.check(Token::Newline) {
                            break;
                        }

                        let value = self.parse_value_ref()?;
                        args.push(value);

                        if !self.check(Token::Comma) {
                            break;
                        }
                        self.advance();
                    }
                }
            }
        }

        Ok((args, immediate, branch_info))
    }

    fn parse_value_ref(&mut self) -> Result<ValueId, String> {
        if let Token::Value(name) = self.peek() {
            let name = name.clone();
            self.advance();

            self.value_map
                .get(&name)
                .copied()
                .ok_or_else(|| format!("Undefined value: {}", name))
        } else {
            Err(format!("Expected value, got {:?}", self.peek()))
        }
    }

    fn parse_block_ref(&mut self) -> Result<BlockId, String> {
        if let Token::BlockName(name) = self.peek() {
            let name = name.clone();
            self.advance();

            // If block doesn't exist yet, create it
            if !self.block_map.contains_key(&name) {
                let block_num = name
                    .trim_start_matches("block")
                    .parse::<u32>()
                    .map_err(|_| format!("Invalid block name: {}", name))?;
                let block_id = BlockId(block_num);
                self.block_map.insert(name.clone(), block_id);
            }

            Ok(self.block_map[&name])
        } else {
            Err(format!("Expected block name, got {:?}", self.peek()))
        }
    }

    fn parse_value_with_type(&mut self) -> Result<(String, Type), String> {
        let name = if let Token::Value(n) = self.peek() {
            n.clone()
        } else {
            return Err(format!("Expected value name, got {:?}", self.peek()));
        };
        self.advance();

        self.expect(Token::Colon)?;

        let ty = self.parse_type()?;

        Ok((name, ty))
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        if let Token::Type(ty) = self.peek() {
            let result = match ty.as_str() {
                "i8" => Type::I8,
                "i16" => Type::I16,
                "i32" => Type::I32,
                "i64" => Type::I64,
                _ => return Err(format!("Unknown type: {}", ty)),
            };
            self.advance();
            Ok(result)
        } else {
            Err(format!("Expected type, got {:?}", self.peek()))
        }
    }

    fn parse_opcode(&self, opcode_str: &str) -> Result<Opcode, String> {
        match opcode_str {
            "iadd" => Ok(Opcode::Add),
            "isub" => Ok(Opcode::Sub),
            "imul" => Ok(Opcode::Mul),
            "idiv" => Ok(Opcode::Div),
            "iadd_imm" => Ok(Opcode::AddImm),
            "imul_imm" => Ok(Opcode::MulImm),
            "ishl_imm" => Ok(Opcode::ShlImm),
            "band" | "and" => Ok(Opcode::And),
            "bor" | "or" => Ok(Opcode::Or),
            "bxor" | "xor" => Ok(Opcode::Xor),
            "ishl" => Ok(Opcode::Shl),
            "ushr" => Ok(Opcode::Ushr),
            "sshr" => Ok(Opcode::Sshr),
            "bnot" => Ok(Opcode::Bnot),
            "ineg" => Ok(Opcode::Ineg),
            "icmp" => Ok(Opcode::Eq), // Will be refined with condition suffix
            "iconst" => Ok(Opcode::Const),
            "uextend" => Ok(Opcode::Uextend),
            "sextend" => Ok(Opcode::Sextend),
            "load" => Ok(Opcode::Load),
            "store" => Ok(Opcode::Store),
            "call" => Ok(Opcode::Call),
            "jump" => Ok(Opcode::Branch),
            "brif" | "br_if" => Ok(Opcode::CondBranch),
            "return" => Ok(Opcode::Return),
            "trap" => Ok(Opcode::Trap),
            _ => Err(format!("Unknown opcode: {}", opcode_str)),
        }
    }

    fn parse_comparison_condition(&self, cond: &str) -> Result<Opcode, String> {
        match cond {
            "eq" => Ok(Opcode::Eq),
            "ne" => Ok(Opcode::Ne),
            "slt" => Ok(Opcode::Slt),
            "sle" => Ok(Opcode::Sle),
            "sgt" => Ok(Opcode::Sgt),
            "sge" => Ok(Opcode::Sge),
            "ult" => Ok(Opcode::Ult),
            "ule" => Ok(Opcode::Ule),
            "ugt" => Ok(Opcode::Ugt),
            "uge" => Ok(Opcode::Uge),
            _ => Err(format!("Unknown comparison condition: {}", cond)),
        }
    }

    fn check_block_start(&self) -> bool {
        matches!(self.peek(), Token::BlockName(_))
    }

    fn expect_keyword(&mut self, keyword: &str) -> Result<(), String> {
        match self.peek() {
            Token::Function if keyword == "function" => {
                self.advance();
                Ok(())
            }
            Token::Block if keyword == "block" => {
                self.advance();
                Ok(())
            }
            _ => Err(format!(
                "Expected keyword '{}', got {:?}",
                keyword,
                self.peek()
            )),
        }
    }

    fn expect(&mut self, expected: Token) -> Result<(), String> {
        if self.peek() == expected {
            self.advance();
            Ok(())
        } else {
            Err(format!("Expected {:?}, got {:?}", expected, self.peek()))
        }
    }

    fn check(&self, token: Token) -> bool {
        if self.is_at_end() {
            return false;
        }
        std::mem::discriminant(&self.peek()) == std::mem::discriminant(&token)
    }

    fn skip_newlines(&mut self) {
        while !self.is_at_end() && self.peek() == Token::Newline {
            self.advance();
        }
    }

    fn peek(&self) -> Token {
        if self.is_at_end() {
            Token::Eof
        } else {
            self.tokens[self.pos].clone()
        }
    }

    fn peek_ahead(&self, n: usize) -> Token {
        if self.pos + n >= self.tokens.len() {
            Token::Eof
        } else {
            self.tokens[self.pos + n].clone()
        }
    }

    fn advance(&mut self) {
        if !self.is_at_end() {
            self.pos += 1;
        }
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }
}

/// Parse CLIF text into IR
pub fn parse_clif(input: &str) -> Result<(DataFlowGraph, Layout), String> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize()?;

    let mut parser = Parser::new(tokens);
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_function() {
        let input = r#"
function %test(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 1
    v2 = iadd.i32 v0, v1
    return v2
}
"#;

        let result = parse_clif(input);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());

        let (dfg, layout) = result.unwrap();
        assert_eq!(layout.blocks.len(), 1);

        let block = &layout.block_data[&BlockId(0)];
        assert_eq!(block.params.len(), 1);
        assert!(block.insts.len() >= 2); // At least iconst and iadd
    }

    #[test]
    fn test_conditional_branch() {
        let input = r#"
function %test(i32) -> i32 {
block0(v0: i32):
    brif v0, block1(v0), block2(v0)

block1(v1: i32):
    v2 = iconst.i32 1
    return v2

block2(v3: i32):
    v4 = iconst.i32 0
    return v4
}
"#;

        let result = parse_clif(input);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());

        let (dfg, layout) = result.unwrap();
        assert_eq!(layout.blocks.len(), 3);
    }

    #[test]
    fn test_with_comments() {
        let input = r#"
function %test(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 1 ; this is a constant
    v2 = iadd.i32 v0, v1 ; add them
    return v2
}
"#;

        let result = parse_clif(input);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_branch_display() {
        let input = r#"
function %test_branches(i32) -> i32 {
block0(v0: i32):
    v1 = iconst.i32 10
    v2 = icmp.eq.i32 v0, v1
    brif v2, block1(v0), block2(v0)

block1(v3: i32):
    v4 = iadd.i32 v3, v1
    jump block3(v4)

block2(v5: i32):
    v6 = imul.i32 v5, v1
    jump block3(v6)

block3(v7: i32):
    return v7
}
"#;

        let result = parse_clif(input);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());

        let (dfg, layout) = result.unwrap();
        assert_eq!(layout.blocks.len(), 4, "Should have 4 blocks");

        // Display the full CLIF format
        println!("\n{}", "=".repeat(70));
        println!("Full CLIF Display with Branch Targets:");
        println!("{}", "=".repeat(70));
        let clif_output = layout.display(&dfg, "test_branches", &[Type::I32], Some(Type::I32));
        println!("{}", clif_output);
        println!("{}", "=".repeat(70));

        // Check that branch instructions are parsed correctly
        let block0 = &layout.block_data[&BlockId(0)];
        let brif_inst_id = block0
            .insts
            .last()
            .expect("block0 should have instructions");
        let brif_inst = &dfg.insts[brif_inst_id];
        assert_eq!(
            brif_inst.opcode,
            Opcode::CondBranch,
            "Last instruction in block0 should be brif"
        );
        assert!(
            brif_inst.branch_info.is_some(),
            "brif should have branch_info"
        );

        // Verify brif display shows both branch targets
        let brif_display = dfg.display_inst(*brif_inst_id);
        assert!(
            brif_display.contains("brif"),
            "Display should contain 'brif'"
        );
        assert!(
            brif_display.contains("block1"),
            "Display should contain 'block1'"
        );
        assert!(
            brif_display.contains("block2"),
            "Display should contain 'block2'"
        );
        println!("✓ brif display: {}", brif_display);

        // Check jump instruction in block1
        let block1 = &layout.block_data[&BlockId(1)];
        let jump_inst_id = block1
            .insts
            .last()
            .expect("block1 should have instructions");
        let jump_inst = &dfg.insts[jump_inst_id];
        assert_eq!(
            jump_inst.opcode,
            Opcode::Branch,
            "Last instruction in block1 should be jump"
        );
        assert!(
            jump_inst.branch_info.is_some(),
            "jump should have branch_info"
        );

        // Verify jump display shows target
        let jump_display = dfg.display_inst(*jump_inst_id);
        assert!(
            jump_display.contains("jump"),
            "Display should contain 'jump'"
        );
        assert!(
            jump_display.contains("block3"),
            "Display should contain 'block3'"
        );
        println!("✓ jump display: {}", jump_display);

        // Check jump instruction in block2
        let block2 = &layout.block_data[&BlockId(2)];
        let jump2_inst_id = block2
            .insts
            .last()
            .expect("block2 should have instructions");
        let jump2_display = dfg.display_inst(*jump2_inst_id);
        assert!(
            jump2_display.contains("jump"),
            "Display should contain 'jump'"
        );
        assert!(
            jump2_display.contains("block3"),
            "Display should contain 'block3'"
        );
        println!("✓ jump display: {}", jump2_display);
    }
}
