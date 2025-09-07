use std::fmt;

use crate::parse::ast;

#[derive(Debug)]
enum Operand {
    Temp(usize),   // temporary, like t1, t2
    Var(String),   // named variable
    Const(i32),    // literal constant
    Label(String), // label for jumps
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Temp(n) => write!(f, "t{}", n),
            Operand::Var(name) => write!(f, "{}", name),
            Operand::Const(value) => write!(f, "{}", value),
            Operand::Label(label) => write!(f, "{}", label),
        }
    }
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

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operator::Add => write!(f, "+"),
            Operator::Sub => write!(f, "-"),
            Operator::Mul => write!(f, "*"),
            Operator::Div => write!(f, "/"),
            Operator::Minus => write!(f, "neg"),
            Operator::Assign => write!(f, "="),
            Operator::Goto => write!(f, "goto"),
            Operator::Label => write!(f, "label"),
            Operator::Return(operand) => {
                if let Some(op) = operand {
                    write!(f, "return {}", op)
                } else {
                    write!(f, "return")
                }
            }
            Operator::IfGoto(condition) => write!(f, "if {} goto", condition),
        }
    }
}

#[derive(Debug)]
struct Instruction {
    op: Operator,
    arg1: Option<Operand>,
    arg2: Option<Operand>,
    result: Option<Operand>,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.op {
            Operator::Return(operand) => {
                if let Some(op) = operand {
                    write!(f, "    return {}", op)
                } else {
                    write!(f, "    return")
                }
            }
            Operator::Assign => {
                if let (Some(result), Some(arg1)) = (&self.result, &self.arg1) {
                    write!(f, "    {} = {}", result, arg1)
                } else {
                    write!(f, "    assign instruction (malformed)")
                }
            }
            Operator::Add | Operator::Sub | Operator::Mul | Operator::Div => {
                if let (Some(result), Some(arg1), Some(arg2)) =
                    (&self.result, &self.arg1, &self.arg2)
                {
                    write!(f, "    {} = {} {} {}", result, arg1, self.op, arg2)
                } else {
                    write!(f, "    binary operation (malformed)")
                }
            }
            Operator::Minus => {
                if let (Some(result), Some(arg1)) = (&self.result, &self.arg1) {
                    write!(f, "    {} = -{}", result, arg1)
                } else {
                    write!(f, "    unary minus (malformed)")
                }
            }
            Operator::Goto => {
                if let Some(arg1) = &self.arg1 {
                    write!(f, "    goto {}", arg1)
                } else {
                    write!(f, "    goto (malformed)")
                }
            }
            Operator::Label => {
                if let Some(arg1) = &self.arg1 {
                    write!(f, "{}:", arg1)
                } else {
                    write!(f, "label (malformed)")
                }
            }
            Operator::IfGoto(condition) => {
                if let (Some(arg1), Some(arg2)) = (&self.arg1, &self.arg2) {
                    write!(
                        f,
                        "    if {} {} {} goto {}",
                        arg1,
                        condition,
                        arg2,
                        self.result
                            .as_ref()
                            .map_or("?".to_string(), |r| r.to_string())
                    )
                } else {
                    write!(f, "    if goto (malformed)")
                }
            }
        }
    }
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

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "function {}:", self.name)?;
        for instruction in &self.body {
            writeln!(f, "{}", instruction)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Program {
    body: Vec<Function>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, function) in self.body.iter().enumerate() {
            if i > 0 {
                writeln!(f)?; // Add blank line between functions
            }
            write!(f, "{}", function)?;
        }
        Ok(())
    }
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
            ast::Statement::Function(function) => self.generate_function(function),
            ast::Statement::Return(expression) => {
                self.generate_return(expression, instruction_list)
            }
            ast::Statement::VarDeclaration(var_declaration) => {
                self.generate_var_declaration(var_declaration, instruction_list)
            }
            ast::Statement::Expression(expression) => {
                self.generate_expression(expression, instruction_list);
            }
        }
    }

    fn generate_function(&mut self, function: &ast::Function) {
        let mut current_function = Function::new(&function.name);

        for statement in &function.body {
            self.generate_statement(&statement, &mut current_function.body);
        }

        self.body.push(current_function);
    }

    fn generate_return(
        &mut self,
        returned: &ast::Expression,
        instruction_list: &mut Vec<Instruction>,
    ) {
        let expression = self.generate_expression(returned, instruction_list);

        let instruction = Instruction {
            op: Operator::Return(Some(expression)),
            arg1: None,
            arg2: None,
            result: None,
        };

        instruction_list.push(instruction);
    }

    fn generate_var_declaration(
        &mut self,
        declaration: &ast::VarDeclaration,
        instruction_list: &mut Vec<Instruction>,
    ) {
        let value = self.generate_expression(&declaration.value, instruction_list);

        let instruction = Instruction {
            op: Operator::Assign,
            arg1: Some(value),
            arg2: None,
            result: Some(Operand::Var(declaration.var_name.to_string())),
        };

        instruction_list.push(instruction);
    }

    fn generate_expression(
        &mut self,
        expression: &ast::Expression,
        instruction_list: &mut Vec<Instruction>,
    ) -> Operand {
        Operand::Temp(1)
    }
}
