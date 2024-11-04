use crate::expr::Expr;

pub enum Stmt {
    Expr(Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Print(Expr),
    While(Expr, Box<Stmt>),
    Block(Vec<Stmt>),
    VarDecl(String, Option<Expr>),
}
