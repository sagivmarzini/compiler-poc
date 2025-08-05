use std::{env, fs, io};

mod ast;
mod parser;
mod token;

fn main() {
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

    println!(
        "{:?}",
        token::tokenize(input.as_str()).expect("Error analyzing the equation")
    );
}
