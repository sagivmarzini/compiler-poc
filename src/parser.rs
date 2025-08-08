use std::fmt;

use crate::{ast::*, lexer::Token};

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedToken(Token),
    UnexpectedEOF,
    Custom(String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken(token) => write!(f, "Unexpected token: {:?}", token),
            ParseError::UnexpectedEOF => write!(f, "Unexpected end of file"),
            ParseError::Custom(msg) => write!(f, "{}", msg),
        }
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn produce_ast(&mut self) -> Result<Stmt, ParseError> {
        let mut equation = Equation {
            body: Vec::<Stmt>::new(),
        };

        while !self.is_eof() {
            equation.body.push(self.parse_statement()?);
        }

        Ok(Stmt::Equation(equation))
    }

    // Orders of Prescidence:
    //     Additive Expression
    //     Multiplicitave Expression
    //     Primary Expression

    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        // In this arithmetic expression compiler there are no statements, skip to parsing expressions

        // Wrap returned expression in a statement enum pointing to it
        Ok(Stmt::Expr(Box::new(self.parse_expression()?)))
    }
    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        let expression = self.parse_additive_expression();

        match self.peek() {
            Some(Token::EOF | Token::RParen) => expression,
            Some(_) => Err(ParseError::Custom("Missing operator".to_string())),
            None => Err(ParseError::UnexpectedEOF),
        }
    }
    fn parse_additive_expression(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_multiplicative_expression()?;

        while let Some(token) = self.peek() {
            match token {
                Token::Plus | Token::Minus => {
                    // Clone the operator for use in the AST
                    let operator = self.eat().unwrap().clone();
                    let right = self.parse_multiplicative_expression()?;

                    left = Expr::BinaryExpr(BinaryExpr {
                        left: Box::new(left),
                        right: Box::new(right),
                        operator: self.convert_operator_token(&operator)?,
                    });
                }
                _ => break,
            }
        }

        Ok(left)
    }
    fn parse_multiplicative_expression(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_primary()?;

        while let Some(token) = self.peek() {
            match token {
                Token::Star | Token::Slash => {
                    let operator = self.eat().unwrap().clone();
                    let right = self.parse_primary()?;

                    left = Expr::BinaryExpr(BinaryExpr {
                        left: Box::new(left),
                        right: Box::new(right),
                        operator: self.convert_operator_token(&operator)?,
                    })
                }
                _ => break,
            }
        }

        Ok(left)
    }
    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match self.eat() {
            Some(Token::Number(n)) => Ok(Expr::NumericLiteral(NumericLiteral { value: *n })),
            Some(Token::LParen) => {
                let expression = self.parse_expression()?;

                match self.expect(Token::RParen) {
                    Ok(_) => Ok(expression),
                    Err(_) => Err(ParseError::Custom("Expected `)`".to_string())),
                }
            }
            Some(token) => Err(ParseError::UnexpectedToken(token.clone())),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

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
        match self.eat() {
            Some(token) if *token == expected => Ok(token),
            Some(token) => Err(ParseError::UnexpectedToken(token.clone())),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn is_eof(&self) -> bool {
        match self.tokens[self.pos] {
            Token::EOF => true,
            _ => false,
        }
    }

    fn convert_operator_token(&self, token: &Token) -> Result<Operator, ParseError> {
        match token {
            Token::Plus => Ok(Operator::Plus),
            Token::Minus => Ok(Operator::Minus),
            Token::Star => Ok(Operator::Multiply),
            Token::Slash => Ok(Operator::Divide),
            _ => Err(ParseError::UnexpectedToken(token.clone())),
        }
    }
}
