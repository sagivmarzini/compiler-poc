use std::{io::Write, process::Command};

use super::snippets;
use crate::parse::ast::{
    self, BinaryExpression, BinaryOperator::*, Expression, Function, Program, Statement,
    UnaryExpression,
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

    pub fn write_program(&mut self, program: &Program) {
        self.write_line(snippets::PROGRAM_PROLOGUE);

        for stmt in &program.body {
            self.write_stmt(stmt, &Context::Program);
        }
    }

    fn write_stmt(&mut self, stmt: &Statement, context: &Context) {
        match stmt {
            Statement::Function(function) => self.write_function(function),
            Statement::Return(returned) => self.write_return(returned, context),
            Statement::Expression(expr) => self.write_expr(expr),
            Statement::VarDeclaration(var_declaration) => todo!(),
        }
    }

    fn write_function(&mut self, func: &Function) {
        self.start_label(&func.name);

        let function_context = Context::Function(func.name.clone());
        for stmt in &func.body {
            self.write_stmt(stmt, &function_context);
        }

        self.close_label();
    }

    fn write_return(&mut self, returned_expr: &Expression, context: &Context) {
        match context {
            Context::Function(name) if name == "main" => {
                self.write_expr(returned_expr);
                self.write_line("mov rdi, rax    ; exit code");
                self.write_line("mov rax, 60  ; syscall: exit");
                self.write_line("syscall");
            }
            Context::Function(_) => {
                self.write_expr(returned_expr);
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
    fn write_expr(&mut self, expr: &Expression) {
        match expr {
            Expression::IntegerLiteral(num) => self.write_line(&format!("mov rax, {}", num)),
            Expression::Identifier(identifier) => {
                self.write_line(&format!("mov rax, [{}]", identifier));
                // later when supporting vars
            }
            Expression::UnaryExpr(unary_operation) => self.write_unary_operation(unary_operation),
            Expression::BinaryExpr(binary_expression) => {
                self.write_binary_operation(binary_expression)
            }
        }
    }

    fn write_unary_operation(&mut self, unary_operation: &UnaryExpression) {
        self.write_expr(&unary_operation.operand);

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

    fn write_binary_operation(&mut self, binary_expression: &BinaryExpression) {
        // Evaluate right-hand side first
        self.write_expr(&binary_expression.right);
        self.write_line("push rax");

        // Then evaluate left-hand side
        self.write_expr(&binary_expression.left);
        self.write_line("pop rcx");

        // RAX = left
        // RCX = right

        match binary_expression.operator {
            Plus => self.write_line("add rax, rcx"),
            Multiply => self.write_line("imul rax, rcx"),
            Minus => {
                self.write_line("sub rax, rcx");
            }
            Divide => {
                self.write_line("cqo");
                self.write_line("idiv rcx");
            }

            Or => self.write_logical_or_operator(),
            And => self.write_logical_and_operator(),

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
                self.write_line("cmp rax, rcx");
                self.write_line("setl al");
                self.write_line("movzx rax, al");
            }
            LessEqual => {
                self.write_line("cmp rax, rcx");
                self.write_line("setle al");
                self.write_line("movzx rax, al");
            }
            Greater => {
                self.write_line("cmp rax, rcx");
                self.write_line("setg al");
                self.write_line("movzx rax, al");
            }
            GreaterEqual => {
                self.write_line("cmp rax, rcx");
                self.write_line("setge al");
                self.write_line("movzx rax, al");
            }
        }
    }

    fn write_logical_or_operator(&mut self) {
        let true_label = self.generate_label("true");
        let false_label = self.generate_label("false");
        let end_label = self.generate_label("end");

        self.write_line("cmp rcx, 0");
        self.write_jump_instruction("jne", &true_label);
        self.write_line("cmp rax, 0");
        self.write_jump_instruction("je", &false_label);

        self.start_label(&true_label);
        self.write_line("mov rax, 1");
        self.write_jump_instruction("jmp", &end_label);
        self.close_label();

        self.start_label(&false_label);
        self.write_line("mov rax, 0");
        self.close_label();

        self.start_label(&end_label);
        self.close_label();
    }

    fn write_logical_and_operator(&mut self) {
        let false_label = self.generate_label("false");
        let end_label = self.generate_label("end");

        self.write_line("cmp rcx, 0");
        self.write_jump_instruction("je", &false_label);
        self.write_line("cmp rax, 0");
        self.write_jump_instruction("je", &false_label);
        self.write_line("mov eax, 1");
        self.write_jump_instruction("jmp", &end_label);

        self.start_label(&false_label);
        self.write_line("mov rax, 0");
        self.close_label();

        self.start_label(&end_label);
        self.close_label();
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

    /// Decrements the indentation by one
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

    fn write_jump_instruction(&mut self, instruction: &str, label: &str) {
        self.write_line(&format!("{} {}", instruction, label));
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
