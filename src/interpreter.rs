use crate::ast::{BinaryExpr, Equation, Expr, Operator, Stmt};

#[derive(Debug, PartialEq)]
pub enum RuntimeValue {
    Number(i64),
    Null,
}

pub fn evaluate(node: &Stmt) -> RuntimeValue {
    match node {
        Stmt::Equation(equation) => evaluate_equation_node(equation),
        Stmt::Expr(expr) => evaluate_expression(&**expr),
    }
}

fn evaluate_expression(expr: &Expr) -> RuntimeValue {
    match expr {
        Expr::BinaryExpr(binary_expr) => evaluate_binary_expression(binary_expr),
        Expr::NumericLiteral(numeric_literal) => RuntimeValue::Number(numeric_literal.value),
    }
}

fn evaluate_equation_node(equation: &Equation) -> RuntimeValue {
    let mut last_evaluated: RuntimeValue = RuntimeValue::Null;

    for child in &equation.body {
        last_evaluated = evaluate(&child);
    }

    last_evaluated
}

fn evaluate_binary_expression(expr: &BinaryExpr) -> RuntimeValue {
    let lhs = evaluate_expression(&*expr.left);
    let rhs = evaluate_expression(&*expr.right);

    // If both lhs and rhs are numbers, we can calculate the result of the operation
    if let (RuntimeValue::Number(left), RuntimeValue::Number(right)) = (&lhs, &rhs) {
        match expr.operator {
            Operator::Plus => RuntimeValue::Number(left + right),
            Operator::Minus => RuntimeValue::Number(left - right),
            Operator::Multiply => RuntimeValue::Number(left * right),
            Operator::Divide => RuntimeValue::Number(left / right),
        }
    } else {
        RuntimeValue::Null
    }
}
