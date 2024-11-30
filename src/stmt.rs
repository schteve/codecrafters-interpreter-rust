use crate::{expr::Expr, token::Token};

#[derive(Clone)]
pub enum Stmt {
    Expr(Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Print(Expr),
    While(Expr, Box<Stmt>),
    Block(Vec<Stmt>),
    VarDecl(String, Option<Expr>),
    FunDecl(String, Vec<String>, Vec<Stmt>),
    Return(Token, Option<Expr>),
}
