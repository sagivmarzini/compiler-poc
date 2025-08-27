use std::collections::VecDeque;
use thiserror::Error;

use super::ast::{
    BinaryExpression, BinaryOperator, Expression, Function, Program, Statement, UnaryExpression,
    UnaryOperator,
};
use crate::lex::token::Keyword::*;
use crate::lex::token::Token;
use crate::parse::ast::VarDeclaration;

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
    #[error("Expected '=' or ';' token after variable declaration")]
    ExpectedAssignmentOrSemicolon,
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
    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        let token = self.peek().ok_or(ParseError::UnexpectedEOF)?;

        match token {
            Token::Keyword(keyword) => match keyword {
                Function => {
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

                    Ok(Statement::Function(Function {
                        name: func_name,
                        body,
                    }))
                }
                Return => {
                    self.eat(); // 'return' keyword

                    let expr = self.parse_expression()?;
                    self.expect(Token::Semicolon)?;

                    Ok(Statement::Return(Box::new(expr)))
                }
                Var => {
                    self.eat(); // 'var' keyword

                    let var_name = self.expect_identifier()?;
                    let var_value: Box<Expression>;

                    match self.peek() {
                        Some(Token::Assignment) => {
                            self.eat();

                            let expression = self.parse_expression()?;
                            var_value = Box::new(expression);

                            self.expect(Token::Semicolon)?;
                        }
                        Some(Token::Semicolon) => {
                            self.eat();

                            // Uninitialized variable defaults to 0
                            var_value = Box::new(Expression::IntegerLiteral(0));
                        }
                        Some(_) => return Err(ParseError::ExpectedAssignmentOrSemicolon),
                        None => return Err(ParseError::ExpectedAssignmentOrSemicolon),
                    }

                    Ok(Statement::VarDeclaration(VarDeclaration {
                        var_name,
                        value: var_value,
                    }))
                }
            },
            _ => Err(ParseError::UnexpectedToken(token.clone())),
        }
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        self.parse_logic_or_expression()
    }

    fn parse_logic_or_expression(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_logic_and_expression()?;

        while let Some(operator) = self.peek().and_then(Parser::token_to_binary_operator) {
            if !matches!(operator, BinaryOperator::And) {
                break;
            }

            self.eat(); // Consume the operator

            let right = self.parse_logic_and_expression()?;

            left = Expression::BinaryExpr(BinaryExpression {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            });
        }

        Ok(left)
    }

    fn parse_logic_and_expression(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_equality_expression()?;

        while let Some(operator) = self.peek().and_then(Parser::token_to_binary_operator) {
            if !matches!(operator, BinaryOperator::Or) {
                break;
            }

            self.eat(); // Consume the operator

            let right = self.parse_equality_expression()?;

            left = Expression::BinaryExpr(BinaryExpression {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            });
        }

        Ok(left)
    }

    fn parse_equality_expression(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_comparison_expression()?;

        while let Some(operator) = self.peek().and_then(Parser::token_to_binary_operator) {
            if !matches!(operator, BinaryOperator::Equal | BinaryOperator::NotEqual) {
                break;
            }

            self.eat(); // Consume the operator

            let right = self.parse_comparison_expression()?;

            left = Expression::BinaryExpr(BinaryExpression {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            });
        }

        Ok(left)
    }

    fn parse_comparison_expression(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_additive_expression()?;

        while let Some(operator) = self.peek().and_then(Parser::token_to_binary_operator) {
            if !matches!(
                operator,
                BinaryOperator::Less
                    | BinaryOperator::LessEqual
                    | BinaryOperator::Greater
                    | BinaryOperator::GreaterEqual
            ) {
                break;
            }

            self.eat(); // Consume the operator

            let right = self.parse_additive_expression()?;

            left = Expression::BinaryExpr(BinaryExpression {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            });
        }

        Ok(left)
    }

    fn parse_additive_expression(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_multiplicative_expression()?;

        while let Some(operator) = self.peek().and_then(Parser::token_to_binary_operator) {
            if !matches!(operator, BinaryOperator::Plus | BinaryOperator::Minus) {
                break;
            }

            self.eat(); // Consume the operator

            let right = self.parse_multiplicative_expression()?;

            left = Expression::BinaryExpr(BinaryExpression {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            });
        }

        Ok(left)
    }

    fn parse_multiplicative_expression(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_unary_expression()?;

        while let Some(operator) = self.peek().and_then(Parser::token_to_binary_operator) {
            if !matches!(operator, BinaryOperator::Multiply | BinaryOperator::Divide) {
                break;
            }

            self.eat(); // Consume the operator

            let right = self.parse_unary_expression()?;

            left = Expression::BinaryExpr(BinaryExpression {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            })
        }

        Ok(left)
    }

    fn parse_unary_expression(&mut self) -> Result<Expression, ParseError> {
        if let Some(token) = self.peek() {
            if let Some(operator) = Parser::token_to_unary_operator(token) {
                self.eat(); // consume the operator

                let operand = self.parse_unary_expression()?; // recursive for chains

                return Ok(Expression::UnaryExpr(UnaryExpression {
                    operator,
                    operand: Box::new(operand),
                }));
            }
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expression, ParseError> {
        match self.eat() {
            Some(Token::Integer(num)) => Ok(Expression::IntegerLiteral(num)),
            Some(Token::Identifier(identifier)) => Ok(Expression::Identifier(identifier)),
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
    /// Eats the next token and returns an error if it's not the expected token
    fn expect(&mut self, expect: Token) -> Result<Token, ParseError> {
        let token = self.eat();

        match token {
            Some(token) if token == expect => Ok(token),
            Some(_) => Err(ParseError::ExpectedToken(expect)),
            None => Err(ParseError::UnexpectedEOF),
        }
    }
    fn expect_identifier(&mut self) -> Result<String, ParseError> {
        match self.eat() {
            Some(Token::Identifier(name)) => Ok(name),
            Some(other) => Err(ParseError::UnexpectedToken(other)),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn token_to_unary_operator(token: &Token) -> Option<UnaryOperator> {
        Some(match token {
            Token::Minus => UnaryOperator::Negative,
            Token::Exclamation => UnaryOperator::Not,
            _ => return None,
        })
    }

    fn token_to_binary_operator(token: &Token) -> Option<BinaryOperator> {
        Some(match token {
            Token::Plus => BinaryOperator::Plus,
            Token::Minus => BinaryOperator::Minus,
            Token::Star => BinaryOperator::Multiply,
            Token::Slash => BinaryOperator::Divide,
            Token::And => BinaryOperator::And,
            Token::Or => BinaryOperator::Or,
            Token::Equal => BinaryOperator::Equal,
            Token::NotEqual => BinaryOperator::NotEqual,
            Token::Less => BinaryOperator::Less,
            Token::LessEqual => BinaryOperator::LessEqual,
            Token::Greater => BinaryOperator::Greater,
            Token::GreaterEqual => BinaryOperator::GreaterEqual,
            _ => return None,
        })
    }
}
