use super::token::{Keyword, Token};

pub struct Lexer {
    pos: usize,
    source: Vec<char>,
}

impl Lexer {
    pub fn new(source_code: Vec<char>) -> Self {
        Lexer {
            pos: 0,
            source: source_code,
        }
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::<Token>::new();

        let mut line_index = 0;
        let mut inline_pos = 0;

        while self.pos < self.source.len() {
            let current = self.eat().unwrap();

            match current {
                '{' => tokens.push(Token::LBrace),
                '}' => tokens.push(Token::RBrace),
                '(' => tokens.push(Token::LParen),
                ')' => tokens.push(Token::RParen),
                ';' => tokens.push(Token::Semicolon),
                '\t' | ' ' => {} // Skip whitespace
                '\n' => {
                    line_index += 1;
                    inline_pos = 0
                }
                '0'..='9' => tokens.push(Token::Integer(self.read_number(current)?)),
                'a'..='z' => match self.read_string(current)?.as_str() {
                    "function" => tokens.push(Token::Keyword(Keyword::Function)),
                    "return" => tokens.push(Token::Keyword(Keyword::Return)),
                    string => tokens.push(Token::Identifer(String::from(string))),
                },
                _ => {
                    return Err(format!(
                        "Unexpected character '{}' at line {}, column {}",
                        current,
                        line_index + 1,
                        inline_pos
                    ));
                }
            }

            inline_pos += 1;
        }

        tokens.push(Token::EOF);

        Ok(tokens)
    }

    fn peek(&self) -> Option<char> {
        self.source.get(self.pos).copied()
    }

    fn eat(&mut self) -> Option<char> {
        let result = self.source.get(self.pos).copied();
        self.pos += 1;

        result
    }

    fn read_number(&mut self, first: char) -> Result<i64, String> {
        let mut num_str = String::new();
        num_str.push(first);

        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() {
                num_str.push(self.eat().unwrap());
            } else {
                break;
            }
        }

        Ok(num_str.parse::<i64>().unwrap())
    }

    fn read_string(&mut self, first: char) -> Result<String, String> {
        let mut string = String::new();
        string.push(first);

        while let Some(ch) = self.peek() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                string.push(self.eat().unwrap());
            } else {
                break;
            }
        }

        Ok(string)
    }
}
