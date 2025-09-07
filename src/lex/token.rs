#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    LBrace,
    RBrace,
    LParen,
    RParen,
    Semicolon,
    Minus,
    Exclamation,
    Plus,
    Star,
    Slash,
    And,
    Or,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Assignment, // '='
    Keyword(Keyword),
    Identifier(String),
    Integer(i32),
    EOF,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Function,
    Return,
    Var,
}
