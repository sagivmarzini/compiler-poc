#[derive(Debug)]
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

#[derive(Debug)]
pub enum Stmt {
    Function(Function),
    Return(Box<Expr>),
    Expr(Box<Expr>),
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub body: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Expr {
    IntegerLiteral(i64),
    Identifier(String),
    UnaryExpr(UnaryExpression),
    BinaryExpr(BinaryExpression),
}

#[derive(Debug)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub operand: Box<Expr>,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Negative,
    Not,
}

#[derive(Debug)]
pub struct BinaryExpression {
    pub left: Box<Expr>,
    pub operator: BinaryOperator,
    pub right: Box<Expr>,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
}
