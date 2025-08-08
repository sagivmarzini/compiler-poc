use std::{env, fs, io};

mod ast;
mod lexer;
mod parser;

use parser::Parser;

fn main() {
    // # Get math equation from the file in the arguments, fallback to stdin

    let args: Vec<String> = env::args().collect();
    let mut input = String::new();
    // If no file was provided, read from stdin
    if args.len() < 2 {
        println!("Enter an Arithmetic equation: ");

        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line from user");
    } else {
        input = fs::read_to_string(&args[1]).expect("Error reading file");
    }

    // # Tokenize the input into a list of tokens
    let tokens = lexer::tokenize(input.as_str()).expect("Error tokenizing the input");
    println!("{:?}", tokens);

    // # Parse the list of tokens into an AST (Abstract Syntax Tree)
    let mut parser = Parser::new(tokens);
    let equation = parser.produce_ast().expect("Error while parsing");
    println!("\n{:?}", equation)
}
