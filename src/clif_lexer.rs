//! CLIF Lexer - Tokenizes Cranelift IR text format

use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
    Function,
    Block,
    
    // Types
    Type(String), // i8, i16, i32, i64
    
    // Identifiers
    FuncName(String),  // %name
    Value(String),     // v0, v1, etc.
    BlockName(String), // block0, block1, etc.
    
    // Opcodes
    Opcode(String),
    
    // Literals
    Integer(i64),
    
    // Punctuation
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Colon,
    Equals,
    Arrow,      // ->
    Dot,
    
    // Special
    Newline,
    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Function => write!(f, "function"),
            Token::Block => write!(f, "block"),
            Token::Type(s) => write!(f, "{}", s),
            Token::FuncName(s) => write!(f, "%{}", s),
            Token::Value(s) => write!(f, "{}", s),
            Token::BlockName(s) => write!(f, "{}", s),
            Token::Opcode(s) => write!(f, "{}", s),
            Token::Integer(n) => write!(f, "{}", n),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::Equals => write!(f, "="),
            Token::Arrow => write!(f, "->"),
            Token::Dot => write!(f, "."),
            Token::Newline => write!(f, "\\n"),
            Token::Eof => write!(f, "EOF"),
        }
    }
}

pub struct Lexer {
    input: Vec<char>,
    pos: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            pos: 0,
        }
    }
    
    pub fn tokenize(&mut self) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();
        
        loop {
            self.skip_whitespace_except_newline();
            
            if self.is_at_end() {
                tokens.push(Token::Eof);
                break;
            }
            
            let token = self.next_token()?;
            
            // Skip consecutive newlines (keep only one)
            if token == Token::Newline {
                if tokens.last() != Some(&Token::Newline) {
                    tokens.push(token);
                }
            } else {
                tokens.push(token);
            }
        }
        
        Ok(tokens)
    }
    
    fn next_token(&mut self) -> Result<Token, String> {
        // Skip comments
        if self.peek() == ';' {
            self.skip_comment();
            return Ok(Token::Newline);
        }
        
        let ch = self.peek();
        
        match ch {
            '\n' => {
                self.advance();
                Ok(Token::Newline)
            }
            '(' => {
                self.advance();
                Ok(Token::LParen)
            }
            ')' => {
                self.advance();
                Ok(Token::RParen)
            }
            '{' => {
                self.advance();
                Ok(Token::LBrace)
            }
            '}' => {
                self.advance();
                Ok(Token::RBrace)
            }
            ',' => {
                self.advance();
                Ok(Token::Comma)
            }
            ':' => {
                self.advance();
                Ok(Token::Colon)
            }
            '=' => {
                self.advance();
                Ok(Token::Equals)
            }
            '.' => {
                self.advance();
                Ok(Token::Dot)
            }
            '-' => {
                self.advance();
                if self.peek() == '>' {
                    self.advance();
                    Ok(Token::Arrow)
                } else {
                    // Negative number
                    self.pos -= 1;
                    self.read_integer()
                }
            }
            '%' => {
                self.advance();
                let name = self.read_identifier();
                Ok(Token::FuncName(name))
            }
            'v' if self.peek_ahead(1).is_ascii_digit() => {
                // Value like v0, v1, etc.
                let name = self.read_identifier();
                Ok(Token::Value(name))
            }
            'b' if self.starts_with("block") => {
                // Block name like block0, block1, etc.
                let name = self.read_identifier();
                Ok(Token::BlockName(name))
            }
            'i' if matches!(self.peek_ahead(1), '8' | '1' | '3' | '6') => {
                // Type like i8, i16, i32, i64
                let ty = self.read_identifier();
                if matches!(ty.as_str(), "i8" | "i16" | "i32" | "i64") {
                    Ok(Token::Type(ty))
                } else {
                    Ok(Token::Opcode(ty))
                }
            }
            _ if ch.is_ascii_digit() => self.read_integer(),
            _ if ch.is_alphabetic() || ch == '_' => {
                let word = self.read_identifier();
                match word.as_str() {
                    "function" => Ok(Token::Function),
                    "block" => Ok(Token::Block),
                    _ => Ok(Token::Opcode(word)),
                }
            }
            _ => Err(format!("Unexpected character: '{}'", ch)),
        }
    }
    
    fn read_identifier(&mut self) -> String {
        let mut s = String::new();
        while !self.is_at_end() {
            let ch = self.peek();
            if ch.is_alphanumeric() || ch == '_' {
                s.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        s
    }
    
    fn read_integer(&mut self) -> Result<Token, String> {
        let mut s = String::new();
        
        if self.peek() == '-' {
            s.push('-');
            self.advance();
        }
        
        while !self.is_at_end() && self.peek().is_ascii_digit() {
            s.push(self.peek());
            self.advance();
        }
        
        s.parse::<i64>()
            .map(Token::Integer)
            .map_err(|e| format!("Invalid integer: {}", e))
    }
    
    fn skip_whitespace_except_newline(&mut self) {
        while !self.is_at_end() {
            let ch = self.peek();
            if ch == ' ' || ch == '\t' || ch == '\r' {
                self.advance();
            } else {
                break;
            }
        }
    }
    
    fn skip_comment(&mut self) {
        while !self.is_at_end() && self.peek() != '\n' {
            self.advance();
        }
    }
    
    fn starts_with(&self, prefix: &str) -> bool {
        let chars: Vec<char> = prefix.chars().collect();
        for (i, &ch) in chars.iter().enumerate() {
            if self.pos + i >= self.input.len() || self.input[self.pos + i] != ch {
                return false;
            }
        }
        true
    }
    
    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.input[self.pos]
        }
    }
    
    fn peek_ahead(&self, n: usize) -> char {
        if self.pos + n >= self.input.len() {
            '\0'
        } else {
            self.input[self.pos + n]
        }
    }
    
    fn advance(&mut self) {
        if !self.is_at_end() {
            self.pos += 1;
        }
    }
    
    fn is_at_end(&self) -> bool {
        self.pos >= self.input.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_basic_tokens() {
        let input = "function %f(i32) -> i32 {";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        
        assert_eq!(tokens[0], Token::Function);
        assert_eq!(tokens[1], Token::FuncName("f".to_string()));
        assert_eq!(tokens[2], Token::LParen);
        assert_eq!(tokens[3], Token::Type("i32".to_string()));
        assert_eq!(tokens[4], Token::RParen);
        assert_eq!(tokens[5], Token::Arrow);
        assert_eq!(tokens[6], Token::Type("i32".to_string()));
        assert_eq!(tokens[7], Token::LBrace);
    }
    
    #[test]
    fn test_block_and_instruction() {
        let input = "block0(v0: i32):\n    v1 = iconst.i32 42";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        
        assert_eq!(tokens[0], Token::BlockName("block0".to_string()));
        assert_eq!(tokens[1], Token::LParen);
        assert_eq!(tokens[2], Token::Value("v0".to_string()));
        assert_eq!(tokens[3], Token::Colon);
        assert_eq!(tokens[4], Token::Type("i32".to_string()));
        assert_eq!(tokens[5], Token::RParen);
        assert_eq!(tokens[6], Token::Colon);
        assert_eq!(tokens[7], Token::Newline);
        assert_eq!(tokens[8], Token::Value("v1".to_string()));
        assert_eq!(tokens[9], Token::Equals);
        assert_eq!(tokens[10], Token::Opcode("iconst".to_string()));
    }
    
    #[test]
    fn test_comment() {
        let input = "v1 = iconst.i32 1 ; this is a comment\nv2 = iadd v0, v1";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        
        // Comment should be converted to newline
        let iadd_pos = tokens.iter().position(|t| matches!(t, Token::Opcode(s) if s == "iadd")).unwrap();
        assert!(iadd_pos > 0);
    }
}
