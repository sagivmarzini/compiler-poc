use std::{io::Write, process::Command};

use super::snippets;
use crate::parse::ast::{
    self, BinaryExpression, BinaryOperator::*, Expr, Function, Program, Stmt, UnaryExpression,
};

#[derive(Debug, Clone)]
pub enum Context {
    Program,
    Function(String), // Store function name
}

pub struct CodeGenerator {
    output: String,
    label_count: i64,
    indent_count: usize,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            output: String::new(),
            label_count: 0,
            indent_count: 0,
        }
    }

    pub fn generate_program(&mut self, program: &Program) {
        self.write_line(snippets::PROGRAM_PROLOGUE);

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
        self.start_label(&func.name);

        let function_context = Context::Function(func.name.clone());
        for stmt in &func.body {
            self.generate_stmt(stmt, &function_context);
        }

        self.close_label();
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

    /// Generates assembly code for a given expression.
    /// #
    /// *This function assumes `rax` is the target register for the result of the expression.*
    fn generate_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::IntegerLiteral(num) => self.write_line(&format!("mov rax, {}", num)),
            Expr::Identifier(identifier) => {
                self.write_line(&format!("mov rax, [{}]", identifier));
                // later when supporting vars
            }
            Expr::UnaryExpr(unary_operation) => self.generate_unary_operation(unary_operation),
            Expr::BinaryExpr(binary_expression) => {
                self.generate_binary_operation(binary_expression)
            }
        }
    }

    fn generate_unary_operation(&mut self, unary_operation: &UnaryExpression) {
        self.generate_expr(&unary_operation.operand);

        match unary_operation.operator {
            ast::UnaryOperator::Negative => {
                self.write_line("neg rax");
            }
            ast::UnaryOperator::Not => {
                self.write_line("cmp rax, 0");
                self.write_line("sete al");
                self.write_line("movzx rax, al");
            }
        }
    }

    fn generate_binary_operation(&mut self, binary_expression: &BinaryExpression) {
        self.generate_expr(&binary_expression.left);
        self.write_line("push rax");

        self.generate_expr(&binary_expression.right);
        self.write_line("pop rcx");

        match binary_expression.operator {
            Plus => self.write_line("add rax, rcx"),
            Multiply => self.write_line("imul rax, rcx"),
            Minus => {
                self.write_line("sub rcx, rax");
                self.write_line("mov rax, rcx");
            }
            Divide => {
                self.write_line("xchg rax, rcx");
                self.write_line("cqo");
                self.write_line("idiv rcx");
            }

            Or => {
                let check2_label = format!("_check2_{}", self.get_label_count());
                let end_label = format!("_end_{}", self.get_label_count());

                self.write_line("cmp rcx, 0");
                self.write_line(&format!("je {}    ; left is 0, check right", check2_label));
                self.write_line("mov rax, 1    ; we didn't jump, so left is true -> result is 1");
                self.write_line(&format!("jmp {}", end_label));

                self.start_label(&check2_label);
                self.write_line("cmp rax, 0");
                self.write_line("setne al");
                self.write_line("movzx rax, al");
                self.close_label();

                self.start_label(&end_label);
                self.close_label();
            }
            And => {
                let check_right_label = self.generate_label("check_right");
                let false_label = self.generate_label("false");
                let end_label = self.generate_label("end");

                self.write_line("cmp rcx, 0");
                self.write_line(&format!(
                    "jne {}    ; left isn't 0, check right",
                    check_right_label
                ));
                self.write_line(&format!("jmp {}    ; left is 0, && is false", false_label));

                self.start_label(&check_right_label);
                self.write_line("cmp rax, 0");
                self.write_line("setne al");
                self.write_line("movzx rax, al");
                self.write_line(&format!("jmp {}", end_label));
                self.close_label();

                self.start_label(&false_label);
                self.write_line("mov rax, 0    ; result of `&&` in rax");
                self.close_label();

                self.start_label(&end_label);
                self.close_label();
            }
            Equal => {
                self.write_line("cmp rax, rcx");
                self.write_line("sete al");
                self.write_line("movzx rax, al");
            }
            NotEqual => {
                self.write_line("cmp rax, rcx");
                self.write_line("setne al");
                self.write_line("movzx rax, al");
            }
            Less => {
                self.write_line("cmp rcx, rax");
                self.write_line("setl al");
                self.write_line("movzx rax, al");
            }
            LessEqual => {
                self.write_line("cmp rcx, rax");
                self.write_line("setle al");
                self.write_line("movzx rax, al");
            }
            Greater => {
                self.write_line("cmp rcx, rax");
                self.write_line("setg al");
                self.write_line("movzx rax, al");
            }
            GreaterEqual => {
                self.write_line("cmp rcx, rax");
                self.write_line("setge al");
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
        self.output += &"    ".repeat(self.indent_count);
        self.output += line;
        self.newline();
    }

    fn start_label(&mut self, label: &str) {
        self.newline();
        self.write_line(&format!("{}:", label));
        self.indent_count += 1;
    }

    fn close_label(&mut self) {
        self.indent_count -= 1;
    }

    fn get_label_count(&mut self) -> i64 {
        let label = self.label_count;
        self.label_count += 1;

        label
    }

    fn generate_label(&mut self, label: &str) -> String {
        format!("_{}_{}", label, self.get_label_count())
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
