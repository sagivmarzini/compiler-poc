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
    /// Global counter for unique labels
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

    pub fn emit_program(&mut self, program: &Program) {
        self.emit_line(snippets::PROGRAM_PROLOGUE);

        for statement in &program.body {
            self.emit_statement(statement, &Context::Program);
        }
    }

    fn emit_statement(&mut self, statement: &Statement, context: &Context) {
        match statement {
            Statement::Function(function) => self.emit_function(function),
            Statement::Return(returned) => self.emit_return(returned, context),
            Statement::Expression(expression) => self.emit_expression_to_register(expression),
            Statement::VarDeclaration(var_declaration) => todo!(),
        }
    }

    fn emit_function(&mut self, func: &Function) {
        self.enter_label(&func.name);

        let function_context = Context::Function(func.name.clone());
        for statement in &func.body {
            self.emit_statement(statement, &function_context);
        }

        self.exit_label();
    }

    fn emit_return(&mut self, returned: &Expression, context: &Context) {
        match context {
            Context::Function(name) if name == "main" => {
                self.emit_expression_to_register(returned);
                self.emit_line("mov rdi, rax    ; exit code");
                self.emit_line("mov rax, 60  ; syscall: exit");
                self.emit_line("syscall");
            }
            Context::Function(_) => {
                self.emit_expression_to_register(returned);
                self.newline();
                self.emit_line("ret");
            }
            Context::Program => {
                panic!("Invalid syntax: return cannot be at program level");
            }
        }
    }

    /// Generates assembly code for a given expression.
    /// #
    /// *This function assumes `rax` is the target register for the result of the expression.*
    fn emit_expression_to_register(&mut self, expression: &Expression) {
        match expression {
            Expression::IntegerLiteral(num) => self.emit_line(&format!("mov rax, {}", num)),
            Expression::Identifier(identifier) => {
                self.emit_line(&format!("mov rax, [{}]", identifier));
                // later when supporting vars
            }
            Expression::UnaryExpression(unary_operation) => {
                self.emit_unary_operation(unary_operation)
            }
            Expression::BinaryExpression(binary_expression) => {
                self.emit_binary_operation(binary_expression)
            }
        }
    }

    fn emit_unary_operation(&mut self, unary_operation: &UnaryExpression) {
        self.emit_expression_to_register(&unary_operation.operand);

        match unary_operation.operator {
            ast::UnaryOperator::Negative => {
                self.emit_line("neg rax");
            }
            ast::UnaryOperator::Not => {
                self.emit_line("cmp rax, 0");
                self.emit_line("sete al");
                self.emit_line("movzx rax, al");
            }
        }
    }

    fn emit_binary_operation(&mut self, binary_expression: &BinaryExpression) {
        // Evaluate right-hand side first
        self.emit_expression_to_register(&binary_expression.right);
        self.emit_line("push rax");

        // Then evaluate left-hand side
        self.emit_expression_to_register(&binary_expression.left);
        self.emit_line("pop rcx");

        // RAX = left
        // RCX = right

        match binary_expression.operator {
            Plus => self.emit_line("add rax, rcx"),
            Multiply => self.emit_line("imul rax, rcx"),
            Minus => {
                self.emit_line("sub rax, rcx");
            }
            Divide => {
                self.emit_line("cqo");
                self.emit_line("idiv rcx");
            }

            Or => self.emit_logical_or_operator(),
            And => self.emit_logical_and_operator(),

            Equal => {
                self.emit_line("cmp rax, rcx");
                self.emit_line("sete al");
                self.emit_line("movzx rax, al");
            }
            NotEqual => {
                self.emit_line("cmp rax, rcx");
                self.emit_line("setne al");
                self.emit_line("movzx rax, al");
            }
            Less => {
                self.emit_line("cmp rax, rcx");
                self.emit_line("setl al");
                self.emit_line("movzx rax, al");
            }
            LessEqual => {
                self.emit_line("cmp rax, rcx");
                self.emit_line("setle al");
                self.emit_line("movzx rax, al");
            }
            Greater => {
                self.emit_line("cmp rax, rcx");
                self.emit_line("setg al");
                self.emit_line("movzx rax, al");
            }
            GreaterEqual => {
                self.emit_line("cmp rax, rcx");
                self.emit_line("setge al");
                self.emit_line("movzx rax, al");
            }
        }
    }

    fn emit_logical_or_operator(&mut self) {
        let true_label = self.generate_label("true");
        let false_label = self.generate_label("false");
        let end_label = self.generate_label("end");

        self.emit_line("cmp rcx, 0");
        self.emit_jump_instruction("jne", &true_label);
        self.emit_line("cmp rax, 0");
        self.emit_jump_instruction("je", &false_label);

        self.enter_label(&true_label);
        self.emit_line("mov rax, 1");
        self.emit_jump_instruction("jmp", &end_label);
        self.exit_label();

        self.enter_label(&false_label);
        self.emit_line("mov rax, 0");
        self.exit_label();

        self.enter_label(&end_label);
        self.exit_label();
    }

    fn emit_logical_and_operator(&mut self) {
        let false_label = self.generate_label("false");
        let end_label = self.generate_label("end");

        self.emit_line("cmp rcx, 0");
        self.emit_jump_instruction("je", &false_label);
        self.emit_line("cmp rax, 0");
        self.emit_jump_instruction("je", &false_label);
        self.emit_line("mov eax, 1");
        self.emit_jump_instruction("jmp", &end_label);

        self.enter_label(&false_label);
        self.emit_line("mov rax, 0");
        self.exit_label();

        self.enter_label(&end_label);
        self.exit_label();
    }

    pub fn finalize(&self) -> &str {
        &self.output
    }

    fn newline(&mut self) {
        self.output.push('\n');
    }

    fn emit_line(&mut self, line: &str) {
        self.output += &"    ".repeat(self.indent_count);
        self.output += line;
        self.newline();
    }

    fn enter_label(&mut self, label: &str) {
        self.newline();
        self.emit_line(&format!("{}:", label));
        self.indent_count += 1;
    }

    /// Decrements the indentation by one
    fn exit_label(&mut self) {
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

    fn emit_jump_instruction(&mut self, instruction: &str, label: &str) {
        self.emit_line(&format!("{} {}", instruction, label));
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
