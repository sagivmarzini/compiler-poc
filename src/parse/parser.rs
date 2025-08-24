use std::collections::VecDeque;
use thiserror::Error;

use super::ast::{
    BinaryExpression, BinaryOperator, Expr, Function, Program, Stmt, UnaryExpression, UnaryOperator,
};
use crate::lex::token::Token;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Unexpected token: {0:?}")]
    UnexpectedToken(Token),
    #[error("Expected identifier, instead gotten {0:?}")]
    ExpectedIdentifier(Token),
    #[error("Expected token missing: {0:?}")]
    ExpectedToken(Token),
    #[error("Unexpected end of file")]
    UnexpectedEOF,
    #[error("{0}")]
    Custom(String),
}

pub struct Parser {
    tokens: VecDeque<Token>,
}

impl Parser {
    pub fn new(tokens: VecDeque<Token>) -> Self {
        Parser { tokens }
    }

    pub fn generate_ast(&mut self) -> Result<Program, ParseError> {
        let mut program = Program::new();

        while !self.tokens.is_empty() && !matches!(self.peek(), Some(Token::EOF)) {
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
                        Token::Identifier(name) => name, // grab the string inside
                        _ => return Err(ParseError::ExpectedIdentifier(func_name_token)),
                    };

                    // TODO: Parse function parameters
                    // For now just skip over the parenthesis
                    self.expect(Token::LParen)?;
                    self.expect(Token::RParen)?;

                    self.expect(Token::LBrace)?;

                    let mut body = Vec::new();
                    while !self.check(Token::RBrace) {
                        // peek at next token, stop at '}'
                        body.push(self.parse_statement()?);
                    }
                    self.expect(Token::RBrace)?;

                    Ok(Stmt::Function(Function {
                        name: func_name,
                        body,
                    }))
                }
                crate::lex::token::Keyword::Return => {
                    self.eat(); // 'return' keyword

                    let expr = self.parse_expression()?;
                    self.expect(Token::Semicolon)?;

                    Ok(Stmt::Return(Box::new(expr)))
                }
            },
            _ => Err(ParseError::UnexpectedToken(token.clone())),
        }
    }

    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_additive_expression()
    }

    fn parse_additive_expression(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_multiplicative_expression()?;

        while let Some(operator) = self.peek().and_then(Parser::token_to_binary_operator) {
            if !matches!(operator, BinaryOperator::Plus | BinaryOperator::Minus) {
                break;
            }

            self.eat(); // Consume the operator

            let right = self.parse_multiplicative_expression()?;

            left = Expr::BinaryExpr(BinaryExpression {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            });
        }

        Ok(left)
    }

    fn parse_multiplicative_expression(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_unary_expression()?;

        while let Some(operator) = self.peek().and_then(Parser::token_to_binary_operator) {
            if !matches!(operator, BinaryOperator::Multiply | BinaryOperator::Divide) {
                break;
            }

            self.eat(); // Consume the operator

            let right = self.parse_unary_expression()?;

            left = Expr::BinaryExpr(BinaryExpression {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            })
        }

        Ok(left)
    }

    fn parse_unary_expression(&mut self) -> Result<Expr, ParseError> {
        if let Some(token) = self.peek() {
            if let Some(operator) = Parser::token_to_unary_operator(token) {
                self.eat(); // consume the operator

                let operand = self.parse_unary_expression()?; // recursive for chains

                return Ok(Expr::UnaryExpr(UnaryExpression {
                    operator,
                    operand: Box::new(operand),
                }));
            }
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match self.eat() {
            Some(Token::Integer(num)) => Ok(Expr::IntegerLiteral(num)),
            Some(Token::Identifier(identifier)) => Ok(Expr::Identifier(identifier)),
            Some(token) => Err(ParseError::UnexpectedToken(token.clone())),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

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
    fn expect(&mut self, expect: Token) -> Result<Token, ParseError> {
        let token = self.eat();

        match token {
            Some(token) if token == expect => Ok(token),
            Some(_) => Err(ParseError::ExpectedToken(expect)),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn token_to_unary_operator(token: &Token) -> Option<UnaryOperator> {
        match token {
            Token::Minus => Some(UnaryOperator::Negative),
            Token::Exclamation => Some(UnaryOperator::Not),
            _ => None,
        }
    }

    fn token_to_binary_operator(token: &Token) -> Option<BinaryOperator> {
        match token {
            Token::Plus => Some(BinaryOperator::Plus),
            Token::Minus => Some(BinaryOperator::Minus),
            Token::Star => Some(BinaryOperator::Multiply),
            Token::Slash => Some(BinaryOperator::Divide),
            _ => None,
        }
    }
}
