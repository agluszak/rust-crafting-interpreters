use crate::token::Location;

#[derive(Debug, PartialEq)]
pub enum ExprType {
    Binary(Box<Expr>, BinaryOperator, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Literal),
    Unary(UnaryOperator, Box<Expr>),
    Variable(String),
    Assign(String, Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub struct Expr {
    pub expr_type: ExprType,
    pub location: Location,
}

impl Expr {
    pub fn new(expr_type: ExprType, location: Location) -> Self {
        Self {
            expr_type,
            location,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
    And,
    Or,
}

#[derive(Debug, PartialEq)]
pub enum UnaryOperator {
    BooleanNegate,
    NumericNegate,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil,
}
