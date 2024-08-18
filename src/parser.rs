use anyhow::Result;
use thiserror::Error;

use crate::token::{Token, TokenType};

#[derive(Clone, Debug, Eq, Error, PartialEq)]
pub enum ParseError {
    #[error("No valid expression")]
    NoValidExpr,
}

pub enum Literal {
    // Number(f64),
    // String(String),
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
                // Literal::Number(n) => println!("{n}"),
                // Literal::String(s) => println!("{s}"),
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
        Self {
            tokens,
            current: 0,
        }
    }

    pub fn parse_ast(&mut self) -> Result<Expr, ParseError> {
        self.parse_primary().ok_or(ParseError::NoValidExpr)
    }

    fn parse_primary(&mut self) -> Option<Expr> {
        if self.advance_if_matches(TokenType::True) {
            Some(Expr::Literal(Literal::Bool(true)))
        } else if self.advance_if_matches(TokenType::False) {
            Some(Expr::Literal(Literal::Bool(false)))
        } else if self.advance_if_matches(TokenType::Nil) {
            Some(Expr::Literal(Literal::Nil))
        } else {
            None
        }
    }

    fn peek(&self) -> Option<Token> {
        self.peek_nth(0)
    }

    fn peek_nth(&self, nth: usize) -> Option<Token> {
        let idx = self.current + nth;
        self.tokens.get(idx).cloned()
    }

    fn advance_if<F>(&mut self, f: F) -> bool
    where
        F: Fn(Token) -> bool,
    {
        let b = self.peek().map(f).unwrap_or(false);
        if b {
            self.current += 1;
        }
        b
    }

    fn advance_if_matches(&mut self, ttype: TokenType) -> bool {
        self.advance_if(|t| ttype.matches(&t.ttype))
    }
}
