#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Equation(Equation),
    Expr(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Equation {
    pub body: Vec<Stmt>,
}

// Define expression types using an enum
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    BinaryExpr(BinaryExpr),
    NumericLiteral(NumericLiteral),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub operator: Operator,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NumericLiteral {
    pub value: i64,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
}
