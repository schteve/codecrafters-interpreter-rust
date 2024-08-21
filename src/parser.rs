use anyhow::Result;
use thiserror::Error;

use crate::token::{Token, TokenType};

#[derive(Clone, Debug, Eq, Error, PartialEq)]
pub enum ParseError {
    #[error("No valid expression")]
    NoValidExpr,
}

pub enum Literal {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}

pub enum Expr {
    Literal(Literal),
}

impl Expr {
    pub fn print(&self) {
        match self {
            Self::Literal(lit) => match lit {
                Literal::Number(n) => println!("{n:?}"),
                Literal::String(s) => println!("{s}"),
                Literal::Bool(b) => println!("{b}"),
                Literal::Nil => println!("nil"),
            },
        }
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse_ast(&mut self) -> Result<Expr, ParseError> {
        self.parse_primary().ok_or(ParseError::NoValidExpr)
    }

    fn parse_primary(&mut self) -> Option<Expr> {
        self.advance().and_then(|t| match t.ttype {
            TokenType::Number(n) => Some(Expr::Literal(Literal::Number(n))),
            TokenType::String(s) => Some(Expr::Literal(Literal::String(s))),
            TokenType::True => Some(Expr::Literal(Literal::Bool(true))),
            TokenType::False => Some(Expr::Literal(Literal::Bool(false))),
            TokenType::Nil => Some(Expr::Literal(Literal::Nil)),
            _ => None,
        })
    }

    fn peek(&self) -> Option<Token> {
        self.peek_nth(0)
    }

    fn peek_nth(&self, nth: usize) -> Option<Token> {
        let idx = self.current + nth;
        self.tokens.get(idx).cloned()
    }

    fn advance(&mut self) -> Option<Token> {
        let t = self.peek();
        if t.is_some() {
            self.current += 1;
        }
        t
    }
}
