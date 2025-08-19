#[derive(Debug, PartialEq)]
pub enum Token {
    LBrace,
    RBrace,
    LParen,
    RParen,
    Semicolon,
    Keyword(Keyword),
    Identifer(String),
    Integer(i64),
    EOF,
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    Function,
    Return,
}

/*
    function main() {
        return 0
    }
*/
