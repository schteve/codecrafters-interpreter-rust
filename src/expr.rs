use crate::token::Token;

pub enum Literal {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}

pub enum Unary {
    Negate(Box<Expr>),
    Not(Box<Expr>),
}

pub enum Binary {
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Less(Box<Expr>, Box<Expr>),
    LessEqual(Box<Expr>, Box<Expr>),
    Greater(Box<Expr>, Box<Expr>),
    GreaterEqual(Box<Expr>, Box<Expr>),
    Equal(Box<Expr>, Box<Expr>),
    NotEqual(Box<Expr>, Box<Expr>),
}

pub enum ExprKind {
    Literal(Literal),
    Grouping(Box<Expr>),
    Unary(Unary),
    Binary(Binary),
    Variable(String),
    Assign(String, Box<Expr>),
}

pub struct Expr {
    pub token: Token,
    pub kind: ExprKind,
}

impl Expr {
    pub fn new(token: Token, kind: ExprKind) -> Self {
        Expr { token, kind }
    }

    pub fn print(&self) {
        match &self.kind {
            ExprKind::Literal(lit) => match lit {
                Literal::Number(n) => print!("{n:?}"),
                Literal::String(s) => print!("{s}"),
                Literal::Bool(b) => print!("{b}"),
                Literal::Nil => print!("nil"),
            },
            ExprKind::Grouping(expr) => {
                print!("(group ");
                expr.print();
                print!(")");
            }
            ExprKind::Unary(un) => match un {
                Unary::Negate(expr) => {
                    print!("(- ");
                    expr.print();
                    print!(")");
                }
                Unary::Not(expr) => {
                    print!("(! ");
                    expr.print();
                    print!(")");
                }
            },
            ExprKind::Binary(bin) => match bin {
                Binary::Add(left, right) => {
                    print!("(+ ");
                    left.print();
                    print!(" ");
                    right.print();
                    print!(")");
                }
                Binary::Sub(left, right) => {
                    print!("(- ");
                    left.print();
                    print!(" ");
                    right.print();
                    print!(")");
                }
                Binary::Mul(left, right) => {
                    print!("(* ");
                    left.print();
                    print!(" ");
                    right.print();
                    print!(")");
                }
                Binary::Div(left, right) => {
                    print!("(/ ");
                    left.print();
                    print!(" ");
                    right.print();
                    print!(")");
                }
                Binary::Less(left, right) => {
                    print!("(< ");
                    left.print();
                    print!(" ");
                    right.print();
                    print!(")");
                }
                Binary::LessEqual(left, right) => {
                    print!("(<= ");
                    left.print();
                    print!(" ");
                    right.print();
                    print!(")");
                }
                Binary::Greater(left, right) => {
                    print!("(> ");
                    left.print();
                    print!(" ");
                    right.print();
                    print!(")");
                }
                Binary::GreaterEqual(left, right) => {
                    print!("(>= ");
                    left.print();
                    print!(" ");
                    right.print();
                    print!(")");
                }
                Binary::Equal(left, right) => {
                    print!("(== ");
                    left.print();
                    print!(" ");
                    right.print();
                    print!(")");
                }
                Binary::NotEqual(left, right) => {
                    print!("(!= ");
                    left.print();
                    print!(" ");
                    right.print();
                    print!(")");
                }
            },
            ExprKind::Variable(name) => print!("(var {name})"),
            ExprKind::Assign(name, value) => {
                print!("({name} = ");
                value.print();
                print!(")");
            }
        }
    }
}
