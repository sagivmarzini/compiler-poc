use std::{collections::VecDeque, fmt};

use super::ast::{Expr, Function, Program, Stmt};
use crate::lex::token::Token;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Token),
    ExpectedIdentifier(Token),
    UnexpectedEOF,
    Custom(String),
}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken(token) => write!(f, "Unexpected token: {:?}", token),
            ParseError::UnexpectedEOF => write!(f, "Unexpected end of file"),
            ParseError::Custom(msg) => write!(f, "{}", msg),
            ParseError::ExpectedIdentifier(token) => {
                write!(f, "Expected indentifer, instead gotten {:?}", token)
            }
        }
    }
}

pub struct Parser {
    tokens: VecDeque<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: VecDeque<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    pub fn generate_ast(&mut self) -> Result<Program, ParseError> {
        let mut program = Program::new();

        while !self.tokens.is_empty() {
            program.body.push(self.parse_statement()?);
        }

        Ok(program)
    }

    // Recursive functions
    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        let token = self.peek().ok_or(ParseError::UnexpectedEOF)?;

        match token {
            Token::Keyword(keyword) => match keyword {
                crate::lex::token::Keyword::Function => {
                    self.eat(); // 'function' keyword

                    // Peek or eat the next token and ensure it is an identifier
                    let func_name_token = self.eat().ok_or(ParseError::UnexpectedEOF)?;
                    let func_name = match func_name_token {
                        Token::Identifer(name) => name, // grab the string inside
                        _ => return Err(ParseError::ExpectedIdentifier(func_name_token)),
                    };

                    // TODO: Parse function parameters
                    // For now just skip over the parenthesis
                    self.eat();
                    self.eat();

                    self.eat(); // {

                    let mut body = Vec::new();
                    while !self.check(Token::RBrace) {
                        // peek at next token, stop at '}'
                        body.push(self.parse_statement()?);
                    }
                    self.eat(); // }

                    Ok(Stmt::Function(Function {
                        name: func_name,
                        body,
                    }))
                }
                crate::lex::token::Keyword::Return => {
                    self.eat(); // 'return' keyword

                    let expr = self.parse_expression()?;

                    Ok(Stmt::Return(Box::new(expr)))
                }
            },
            _ => Err(ParseError::UnexpectedToken(token.clone())),
        }
    }

    fn parse_expression(&mut self) -> Result<Expr, ParseError> {}

    // Helper functions
    fn peek(&self) -> Option<&Token> {
        self.tokens.front()
    }
    fn eat(&mut self) -> Option<Token> {
        self.tokens.pop_front()
    }
    fn check(&self, token: Token) -> bool {
        *self.tokens.front().unwrap() == token
    }
}
