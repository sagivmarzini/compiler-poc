use std::collections::VecDeque;

use super::token::{Keyword, Token};

pub struct Lexer {
    source: VecDeque<char>,
    line_index: i32,
    inline_pos: i32,
}

impl Lexer {
    pub fn new(source_code: Vec<char>) -> Self {
        Lexer {
            source: VecDeque::<char>::from_iter(source_code),
            line_index: 0,
            inline_pos: 0,
        }
    }

    pub fn lex(&mut self) -> Result<VecDeque<Token>, String> {
        let mut tokens = VecDeque::<Token>::new();

        while !self.source.is_empty() {
            let current = self.eat().unwrap();

            match current {
                '{' => tokens.push_back(Token::LBrace),
                '}' => tokens.push_back(Token::RBrace),
                '(' => tokens.push_back(Token::LParen),
                ')' => tokens.push_back(Token::RParen),
                ';' => tokens.push_back(Token::Semicolon),
                '\t' | ' ' => {} // Skip whitespace
                '\n' => {
                    self.line_index += 1;
                    self.inline_pos = 0
                }
                '0'..='9' => tokens.push_back(Token::Integer(self.read_number(current)?)),
                'a'..='z' => match self.read_string(current)?.as_str() {
                    "function" => tokens.push_back(Token::Keyword(Keyword::Function)),
                    "return" => tokens.push_back(Token::Keyword(Keyword::Return)),
                    string => tokens.push_back(Token::Identifier(String::from(string))),
                },
                _ => {
                    return Err(format!(
                        "Unexpected character '{}' at line {}, column {}",
                        current,
                        self.line_index + 1,
                        self.inline_pos
                    ));
                }
            }
        }

        tokens.push_back(Token::EOF);

        Ok(tokens)
    }

    fn peek(&self) -> Option<&char> {
        self.source.front()
    }
    fn eat(&mut self) -> Option<char> {
        self.inline_pos += 1;
        self.source.pop_front()
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
            if ch.is_ascii_alphanumeric() || *ch == '_' {
                string.push(self.eat().unwrap());
            } else {
                break;
            }
        }

        Ok(string)
    }
}
