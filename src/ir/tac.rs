use crate::parse::ast;

#[derive(Debug)]
enum Operand {
    Temp(usize),   // temporary, like t1, t2
    Var(String),   // named variable
    Const(i32),    // literal constant
    Label(String), // label for jumps
}

#[derive(Debug)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Minus,
    Assign,
    Goto,
    Label,
    Return(Option<Operand>),
    IfGoto(String), // condition, e.g. "==", "<", etc.
}

#[derive(Debug)]
struct Instruction {
    op: Operator,
    arg1: Option<Operand>,
    arg2: Option<Operand>,
    result: Option<Operand>,
}

#[derive(Debug)]
struct Function {
    name: String,
    body: Vec<Instruction>,
    // params:
}

impl Function {
    fn new(name: &str) -> Self {
        Function {
            name: name.to_string(),
            body: Vec::<Instruction>::new(),
        }
    }
}

#[derive(Debug)]
pub struct Program {
    body: Vec<Function>,
}

pub struct TacGenerator {
    body: Vec<Function>,
}

impl TacGenerator {
    pub fn new() -> Self {
        TacGenerator {
            body: Vec::<Function>::new(),
        }
    }

    pub fn generate_tac(&mut self, program: &ast::Program) {
        let mut global_function = Function::new("__global__");

        for statement in &program.body {
            self.generate_statement(&statement, &mut global_function.body);
        }
    }

    pub fn get_program(self) -> Program {
        Program { body: self.body }
    }

    fn generate_statement(
        &mut self,
        statement: &ast::Statement,
        instruction_list: &mut Vec<Instruction>,
    ) {
        match statement {
            ast::Statement::Function(function) => {
                self.generate_function(function, instruction_list)
            }
            ast::Statement::Return(expression) => {
                self.generate_return(expression, instruction_list)
            }
            ast::Statement::VarDeclaration(var_declaration) => {
                self.generate_var_declaration(var_declaration, instruction_list)
            }
            ast::Statement::Expression(expression) => {
                self.generate_expression(expression, instruction_list)
            }
        }
    }

    fn generate_function(
        &mut self,
        function: &ast::Function,
        instruction_list: &mut Vec<Instruction>,
    ) {
        let mut current_function = Function::new(&function.name);

        for statement in &function.body {
            self.generate_statement(&statement, &mut current_function.body);
        }

        self.body.push(current_function);
    }

    fn generate_return(
        &mut self,
        expression: &ast::Expression,
        instruction_list: &mut Vec<Instruction>,
    ) {
        todo!()
    }

    fn generate_var_declaration(
        &mut self,
        var_declaration: &ast::VarDeclaration,
        instruction_list: &mut Vec<Instruction>,
    ) {
        todo!()
    }

    fn generate_expression(
        &mut self,
        expression: &ast::Expression,
        instruction_list: &mut Vec<Instruction>,
    ) {
        todo!()
    }
}
