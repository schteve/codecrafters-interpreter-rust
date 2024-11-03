use crate::expr::Expr;

pub enum Stmt {
    Expr(Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Print(Expr),
    Block(Vec<Stmt>),
    VarDecl(String, Option<Expr>),
}
