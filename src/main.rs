mod codegen;
mod lex;
mod parse;

use std::{env, fs, process::exit, time::Instant};

use crate::{codegen::generate::CodeGenerator, lex::Lexer, parse::Parser};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("Error: please provide a file to compile");
        exit(1);
    }

    let input = fs::read_to_string(&args[1]).expect("Error reading file");

    let lex_start = Instant::now();
    let mut lexer = Lexer::new(input.chars().collect());
    let tokens = lexer.lex().expect("Error lexing the input file");
    let lex_time = lex_start.elapsed();
    println!("Lexing took: {:.3}ms", lex_time.as_secs_f64() * 1000.0);

    println!("Tokens: \n{:?}\n", tokens);

    let parse_start = Instant::now();
    let mut parser = Parser::new(tokens);
    let program = parser.generate_ast().expect("Error when generating AST");
    let parse_time = parse_start.elapsed();
    println!("Parsing took: {:.3}ms", parse_time.as_secs_f64() * 1000.0);

    println!("AST: \n{:?}\n", program);

    let codegen_start = Instant::now();
    let mut generator = CodeGenerator::new();
    generator.emit_program(&program);
    let codegen_time = codegen_start.elapsed();
    println!("Program: \n{}\n", generator.finalize());

    generator
        .compile()
        .expect("Error generating the Assembly file");
    println!(
        "Code generation took: {:.3}ms",
        codegen_time.as_secs_f64() * 1000.0
    );

    let total_time = lex_time + parse_time + codegen_time;
    println!(
        "\nTotal compilation time: {:.3}ms",
        total_time.as_secs_f64() * 1000.0
    );
}
