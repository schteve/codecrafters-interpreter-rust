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

pub enum Binary {
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Less(Box<Expr>, Box<Expr>),
    LessEqual(Box<Expr>, Box<Expr>),
    Greater(Box<Expr>, Box<Expr>),
    GreaterEqual(Box<Expr>, Box<Expr>),
}

pub enum Expr {
    Literal(Literal),
    Grouping(Box<Expr>),
    Unary(Unary),
    Binary(Binary),
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
            Self::Binary(bin) => match bin {
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
        let mut expr = self.parse_term()?;

        while let Some(Token {
            ttype:
                TokenType::Less | TokenType::LessEqual | TokenType::Greater | TokenType::GreaterEqual,
            ..
        }) = self.peek()
        {
            let operator = self.advance().unwrap();
            let right = self.parse_term().or(Err(ParseError::NoValidExpr))?;
            expr = match operator.ttype {
                TokenType::Less => Expr::Binary(Binary::Less(Box::new(expr), Box::new(right))),
                TokenType::LessEqual => {
                    Expr::Binary(Binary::LessEqual(Box::new(expr), Box::new(right)))
                }
                TokenType::Greater => {
                    Expr::Binary(Binary::Greater(Box::new(expr), Box::new(right)))
                }
                TokenType::GreaterEqual => {
                    Expr::Binary(Binary::GreaterEqual(Box::new(expr), Box::new(right)))
                }
                _ => unreachable!("Invalid type"),
            }
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_factor()?;

        while let Some(Token {
            ttype: TokenType::Plus | TokenType::Minus,
            ..
        }) = self.peek()
        {
            let operator = self.advance().unwrap();
            let right = self.parse_factor().or(Err(ParseError::NoValidExpr))?;
            expr = match operator.ttype {
                TokenType::Plus => Expr::Binary(Binary::Add(Box::new(expr), Box::new(right))),
                TokenType::Minus => Expr::Binary(Binary::Sub(Box::new(expr), Box::new(right))),
                _ => unreachable!("Invalid type"),
            }
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_unary()?;

        while let Some(Token {
            ttype: TokenType::Star | TokenType::Slash,
            ..
        }) = self.peek()
        {
            let operator = self.advance().unwrap();
            let right = self.parse_unary().or(Err(ParseError::NoValidExpr))?;
            expr = match operator.ttype {
                TokenType::Star => Expr::Binary(Binary::Mul(Box::new(expr), Box::new(right))),
                TokenType::Slash => Expr::Binary(Binary::Div(Box::new(expr), Box::new(right))),
                _ => unreachable!("Invalid type"),
            }
        }

        Ok(expr)
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
