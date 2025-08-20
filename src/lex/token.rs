#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    LBrace,
    RBrace,
    LParen,
    RParen,
    Semicolon,
    Keyword(Keyword),
    Identifier(String),
    Integer(i64),
    EOF,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Function,
    Return,
}
