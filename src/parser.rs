use std::fmt::format;

use crate::{ast::*, token::Token};

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedToken(String),
    UnexpectedEOF,
    Custom(String),
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn produce_AST(&mut self) -> Result<Program, ParseError> {}

    // fn parse_program(&mut self) -> Result<Program, ParseError> {}
    // fn parse_statement(&mut self) -> Result<Stmt, ParseError> {}
    // fn parse_expression(&mut self) -> Result<Expr, ParseError> {}
    // fn parse_primary(&mut self) -> Result<Expr, ParseError> {}

    // Helper methods
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn eat(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.pos);
        self.pos += 1;

        token
    }

    fn expect(&mut self, expected: Token) -> Result<&Token, ParseError> {
        match self.tokens.get(self.pos) {
            Some(token) if *token == expected => Ok(token),
            Some(token) => Err(ParseError::UnexpectedToken(format!("{:?}", token))),
            None => Err(ParseError::UnexpectedEOF),
        }
    }
}
