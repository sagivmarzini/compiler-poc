#[derive(Debug)]
pub struct Program {
    pub body: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            body: Vec::<Statement>::new(),
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    Function(Function),
    Return(Box<Expression>),
    VarDeclaration(VarDeclaration),
    Expression(Box<Expression>),
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub enum Expression {
    IntegerLiteral(i64),
    Identifier(String),
    UnaryExpr(UnaryExpression),
    BinaryExpr(BinaryExpression),
}

#[derive(Debug)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub operand: Box<Expression>,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Negative,
    Not,
}

#[derive(Debug)]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub operator: BinaryOperator,
    pub right: Box<Expression>,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    And,
    Or,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug)]
pub struct VarDeclaration {
    pub var_name: String,
    pub value: Box<Expression>,
}
