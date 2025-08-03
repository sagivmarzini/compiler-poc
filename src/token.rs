#[derive(Debug, PartialEq)]
pub enum Token {
    Number(f64),
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    EOF,
}

pub fn tokenize(code: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::<Token>::new();

    let chars: Vec<char> = code.chars().collect();
    let mut index = 0;
    while index < chars.len() {
        let char = chars[index];

        match char {
            '+' => tokens.push(Token::Plus),
            '-' => tokens.push(Token::Minus),
            '*' => tokens.push(Token::Star),
            '/' => tokens.push(Token::Slash),
            '(' => tokens.push(Token::LParen),
            ')' => tokens.push(Token::RParen),
            '0'..='9' => {
                let mut num_str = String::new();
                while index < chars.len() && chars[index].is_ascii_digit() {
                    num_str.push(chars[index]);
                    index += 1;
                }

                // Don't increment index again after match
                index -= 1;

                let number: f64 = num_str.parse().unwrap();
                tokens.push(Token::Number(number));
            }
            ' ' | '\t' => {
                // Skip whitespace
            }
            '\n' => tokens.push(Token::EOF),
            _ => {
                return Err(format!(
                    "Unexpected character '{}' at index {}",
                    char, index
                ));
            }
        }

        index += 1;
    }

    Ok(tokens)
}
