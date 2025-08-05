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

    fn parse_program(&mut self) -> Result<Program, ParseError> {}
    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {}
    fn parse_expression(&mut self) -> Result<Expr, ParseError> {}
    fn parse_primary(&mut self) -> Result<Expr, ParseError> {}

    // Helper methods
    fn peek(&self) -> Option<&Token> {}
    fn eat(&mut self) -> Option<&Token> {}
    fn expect(&mut self, expected: TokenType) -> Result<&Token, ParseError> {}
}
