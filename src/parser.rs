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

pub enum Unary {
    Negate(Box<Expr>),
    Not(Box<Expr>),
}

pub enum Expr {
    Literal(Literal),
    Grouping(Box<Expr>),
    Unary(Unary),
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
            Self::Grouping(expr) => {
                print!("(group ");
                expr.print();
                print!(")");
            }
            Self::Unary(un) => match un {
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
        self.peek()
            .ok_or(ParseError::NoValidExpr)
            .and_then(|t| match t.ttype {
                TokenType::Minus => {
                    self.advance();
                    let expr = self.parse_unary()?;
                    Ok(Expr::Unary(Unary::Negate(Box::new(expr))))
                }
                TokenType::Bang => {
                    self.advance();
                    let expr = self.parse_unary()?;
                    Ok(Expr::Unary(Unary::Not(Box::new(expr))))
                }
                _ => self.parse_primary(),
            })
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        self.peek()
            .ok_or(ParseError::NoValidExpr)
            .and_then(|t| match t.ttype {
                TokenType::Number(n) => {
                    self.advance();
                    Ok(Expr::Literal(Literal::Number(n)))
                }
                TokenType::String(s) => {
                    self.advance();
                    Ok(Expr::Literal(Literal::String(s)))
                }
                TokenType::True => {
                    self.advance();
                    Ok(Expr::Literal(Literal::Bool(true)))
                }
                TokenType::False => {
                    self.advance();
                    Ok(Expr::Literal(Literal::Bool(false)))
                }
                TokenType::Nil => {
                    self.advance();
                    Ok(Expr::Literal(Literal::Nil))
                }
                TokenType::LeftParen => {
                    self.advance();
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
