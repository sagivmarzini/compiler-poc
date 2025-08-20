mod codegen;
mod lex;
mod parse;

use std::{env, fs, process::exit};

use crate::{codegen::generate::CodeGenerator, lex::Lexer, parse::Parser};

/*
    function main() {
        return 0;
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

    println!("Tokens: \n{:?}\n", tokens);

    let mut parser = Parser::new(tokens);
    let program = parser.generate_ast().expect("Error when generating AST");

    println!("AST: \n{:?}\n", program);

    let mut generator = CodeGenerator::new();
    generator.generate_program(&program);

    println!("Program: \n{}\n", generator.finalize());
    generator
        .compile()
        .expect("Error generating the Assembly file");
}
