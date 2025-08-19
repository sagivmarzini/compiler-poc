pub struct Program {
    pub body: Vec<Stmt>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            body: Vec::<Stmt>::new(),
        }
    }
}

pub enum Stmt {
    Function(Function),
    Return(Box<Expr>),

    Expr(Box<Expr>),
}

pub struct Function {
    pub name: String,
    pub body: Vec<Stmt>,
}

pub enum Expr {}
