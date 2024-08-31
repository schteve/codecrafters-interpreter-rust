use anyhow::Result;
use thiserror::Error;

use crate::token::{Token, TokenType};

#[derive(Clone, Debug, Eq, Error, PartialEq)]
pub enum ParseError {
    #[error("No valid expression")]
    NoValidExpr,
    #[error("Expect ')' after expression.")]
    ExpectRightParen,
}

pub enum Literal {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}

pub enum Expr {
    Literal(Literal),
    Grouping(Box<Expr>),
}

impl Expr {
    pub fn print(&self) {
        match self {
            Self::Literal(lit) => match lit {
                Literal::Number(n) => print!("{n:?}"),
                Literal::String(s) => print!("{s}"),
                Literal::Bool(b) => print!("{b}"),
                Literal::Nil => print!("nil"),
            },
            Self::Grouping(grp) => {
                print!("(group ");
                grp.print();
                print!(")");
            }
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
        self.parse_expr()
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_equality()
    }

    fn parse_equality(&mut self) -> Result<Expr, ParseError> {
        self.parse_comparison()
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        self.parse_term()
    }

    fn parse_term(&mut self) -> Result<Expr, ParseError> {
        self.parse_factor()
    }

    fn parse_factor(&mut self) -> Result<Expr, ParseError> {
        self.parse_unary()
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        self.advance()
            .ok_or(ParseError::NoValidExpr)
            .and_then(|t| match t.ttype {
                TokenType::Number(n) => Ok(Expr::Literal(Literal::Number(n))),
                TokenType::String(s) => Ok(Expr::Literal(Literal::String(s))),
                TokenType::True => Ok(Expr::Literal(Literal::Bool(true))),
                TokenType::False => Ok(Expr::Literal(Literal::Bool(false))),
                TokenType::Nil => Ok(Expr::Literal(Literal::Nil)),
                TokenType::LeftParen => {
                    let expr = self.parse_expr()?;
                    match self.advance() {
                        Some(Token {
                            ttype: TokenType::RightParen,
                            ..
                        }) => Ok(Expr::Grouping(Box::new(expr))),
                        _ => Err(ParseError::ExpectRightParen),
                    }
                }
                _ => Err(ParseError::NoValidExpr),
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
