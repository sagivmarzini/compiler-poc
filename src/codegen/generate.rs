use std::{io::Write, process::Command};

use super::snippets;
use crate::parse::ast::{self, Expr, Function, Program, Stmt, UnaryOperation};

pub struct CodeGenerator {
    output: String,
}

#[derive(Debug, Clone)]
pub enum Context {
    Program,
    Function(String), // Store function name
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            output: String::new(),
        }
    }

    pub fn generate_program(&mut self, program: &Program) {
        self.write(snippets::PROGRAM_PROLOGUE);

        for stmt in &program.body {
            self.generate_stmt(stmt, &Context::Program);
        }
    }

    fn generate_stmt(&mut self, stmt: &Stmt, context: &Context) {
        match stmt {
            Stmt::Function(function) => self.generate_function(function),
            Stmt::Return(returned) => self.generate_return(returned, context),
            Stmt::Expr(expr) => self.generate_expr(expr),
        }
    }

    fn generate_function(&mut self, func: &Function) {
        let label: String = match func.name.as_str() {
            "main" => "_start".to_string(),
            _ => func.name.clone(),
        };
        self.newline();
        self.write_line(format!("{}:", label).as_str());

        let function_context = Context::Function(func.name.clone());
        for stmt in &func.body {
            self.generate_stmt(stmt, &function_context);
        }
    }

    fn generate_return(&mut self, returned_expr: &Expr, context: &Context) {
        match context {
            Context::Function(name) if name == "main" => {
                self.generate_expr(returned_expr);
                self.write_line("mov rdi, rax    ; exit code");
                self.write_line("mov rax, 60  ; syscall: exit");
                self.write_line("syscall");
            }
            Context::Function(_) => {
                self.generate_expr(returned_expr);
                self.newline();
                self.write_line("ret");
            }
            Context::Program => {
                panic!("Invalid syntax: return cannot be at program level");
            }
        }
    }

    fn generate_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::IntegerLiteral(num) => {
                self.write_line(&format!("mov rax, {}", num));
            }
            Expr::Identifier(identifier) => {
                self.write_line(&format!("mov rax, [{}]", identifier));
                // later when supporting vars
            }
            Expr::UnaryOperation(unary_operation) => {
                self.generate_unary_operation(unary_operation);
            }
        }
    }

    fn generate_unary_operation(&mut self, unary_operation: &UnaryOperation) {
        self.generate_expr(&unary_operation.operand);

        match unary_operation.operator {
            ast::UnaryOperator::Minus => {
                self.write_line("neg rax");
            }
            ast::UnaryOperator::Not => {
                self.write_line("cmp rax, 0");
                self.write_line("sete al");
                self.write_line("movzx rax, al");
            }
        }
    }

    pub fn finalize(&self) -> &str {
        &self.output
    }

    fn newline(&mut self) {
        self.output.push('\n');
    }

    fn write_line(&mut self, line: &str) {
        self.output += line;
        self.newline();
    }

    fn write(&mut self, code: &str) {
        self.output += code;
    }

    pub fn compile(&self) -> std::io::Result<()> {
        let mut file = std::fs::File::create("out.asm")?;
        file.write_all(self.output.as_bytes())?;

        // NASM assemble
        let assemble = Command::new("nasm")
            .args(&["-f", "elf64", "out.asm", "-o", "out.o"])
            .output()?;
        if !assemble.status.success() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("NASM failed: {}", String::from_utf8_lossy(&assemble.stderr)),
            ));
        }

        // LD link
        let link = Command::new("ld").args(&["out.o", "-o", "out"]).output()?;
        if !link.status.success() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("LD failed: {}", String::from_utf8_lossy(&link.stderr)),
            ));
        }

        // Run
        Command::new("./out").output()?;

        Ok(())
    }
}
