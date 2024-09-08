use crate::expr::Expr;

pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    VarDecl(String, Option<Expr>),
}
