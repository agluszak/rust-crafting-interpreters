use crate::expr::Expr;

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Expression(Box<Expr>),
    Print(Box<Expr>),
    Var(String, Option<Box<Expr>>)
}