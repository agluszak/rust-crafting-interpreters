use crate::expr::Expr;

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    Var(String, Option<Expr>),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Function(String, Vec<String>, Vec<Stmt>),
    While(Expr, Box<Stmt>),
    Return(Option<Expr>),
}
