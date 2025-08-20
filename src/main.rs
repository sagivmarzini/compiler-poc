mod lex;
mod parse;

use std::{env, fs, process::exit};

use crate::{lex::lexer::Lexer, parse::parser::Parser};

/*
    function main() {
        return 0
    }
*/
fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("Error: please provide a file to compile");
        exit(1);
    }

    let input = fs::read_to_string(&args[1]).expect("Error reading file");

    let mut lexer = Lexer::new(input.chars().collect());
    let tokens = lexer.lex().expect("Error lexing the input file");

    println!("Tokens: \n{:?}", tokens);

    let mut parser = Parser::new(tokens);
    let program = parser.generate_ast().expect("Error when generating AST");

    println!("AST: \n{:?}", program);
}
