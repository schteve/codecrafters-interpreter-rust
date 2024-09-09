use crate::expr::Expr;

pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    Block(Vec<Stmt>),
    VarDecl(String, Option<Expr>),
}
