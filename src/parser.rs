use anyhow::Result;
use thiserror::Error;

use crate::token::{Token, TokenType};

#[derive(Clone, Debug, Eq, Error, PartialEq)]
pub enum ParseErrorKind {
    #[error("No valid expression")]
    NoValidExpr,
    #[error("Expect ')' after expression.")]
    ExpectRightParen,
    #[error("Expect expression.")]
    ExpectExpression,
}

#[derive(Clone, Debug, Error, PartialEq)]
#[error("[line {}] Error at {}: {source}",
    if let Some(t) = &self.token { t.line } else { 0 },
    if let Some(t) = &self.token { format!("'{}'", &t.lexeme) } else { String::from("end") }
)]
pub struct ParseError {
    token: Option<Token>,
    source: ParseErrorKind,
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
    Equal(Box<Expr>, Box<Expr>),
    NotEqual(Box<Expr>, Box<Expr>),
}

pub enum ExprKind {
    Literal(Literal),
    Grouping(Box<Expr>),
    Unary(Unary),
    Binary(Binary),
}

pub struct Expr {
    pub token: Token,
    pub kind: ExprKind,
}

impl Expr {
    fn new(token: Token, kind: ExprKind) -> Self {
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
        let mut expr = self.parse_comparison()?;

        while let Some(Token {
            ttype: TokenType::EqualEqual | TokenType::BangEqual,
            ..
        }) = self.peek()
        {
            let operator = self.advance().unwrap();
            let right = self
                .parse_comparison()
                .map_err(|_| self.error(ParseErrorKind::ExpectExpression))?;
            let kind = match operator.ttype {
                TokenType::EqualEqual => {
                    ExprKind::Binary(Binary::Equal(Box::new(expr), Box::new(right)))
                }
                TokenType::BangEqual => {
                    ExprKind::Binary(Binary::NotEqual(Box::new(expr), Box::new(right)))
                }
                _ => unreachable!("Invalid type"),
            };
            expr = Expr::new(operator, kind);
        }

        Ok(expr)
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
            let right = self
                .parse_term()
                .map_err(|_| self.error(ParseErrorKind::ExpectExpression))?;
            let kind = match operator.ttype {
                TokenType::Less => ExprKind::Binary(Binary::Less(Box::new(expr), Box::new(right))),
                TokenType::LessEqual => {
                    ExprKind::Binary(Binary::LessEqual(Box::new(expr), Box::new(right)))
                }
                TokenType::Greater => {
                    ExprKind::Binary(Binary::Greater(Box::new(expr), Box::new(right)))
                }
                TokenType::GreaterEqual => {
                    ExprKind::Binary(Binary::GreaterEqual(Box::new(expr), Box::new(right)))
                }
                _ => unreachable!("Invalid type"),
            };
            expr = Expr::new(operator, kind);
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
            let right = self
                .parse_factor()
                .map_err(|_| self.error(ParseErrorKind::ExpectExpression))?;
            let kind = match operator.ttype {
                TokenType::Plus => ExprKind::Binary(Binary::Add(Box::new(expr), Box::new(right))),
                TokenType::Minus => ExprKind::Binary(Binary::Sub(Box::new(expr), Box::new(right))),
                _ => unreachable!("Invalid type"),
            };
            expr = Expr::new(operator, kind);
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
            let right = self
                .parse_unary()
                .map_err(|_| self.error(ParseErrorKind::ExpectExpression))?;
            let kind = match operator.ttype {
                TokenType::Star => ExprKind::Binary(Binary::Mul(Box::new(expr), Box::new(right))),
                TokenType::Slash => ExprKind::Binary(Binary::Div(Box::new(expr), Box::new(right))),
                _ => unreachable!("Invalid type"),
            };
            expr = Expr::new(operator, kind);
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        self.peek()
            .ok_or_else(|| self.error(ParseErrorKind::NoValidExpr))
            .and_then(|t| match t.ttype {
                TokenType::Minus => {
                    self.advance();
                    let expr = self.parse_unary()?;
                    let kind = ExprKind::Unary(Unary::Negate(Box::new(expr)));
                    Ok(Expr::new(t, kind))
                }
                TokenType::Bang => {
                    self.advance();
                    let expr = self.parse_unary()?;
                    let kind = ExprKind::Unary(Unary::Not(Box::new(expr)));
                    Ok(Expr::new(t, kind))
                }
                _ => self.parse_primary(),
            })
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        self.peek()
            .ok_or_else(|| self.error(ParseErrorKind::NoValidExpr))
            .and_then(|t| match &t.ttype {
                TokenType::Number(n) => {
                    self.advance();
                    let kind: ExprKind = ExprKind::Literal(Literal::Number(*n));
                    Ok(Expr::new(t, kind))
                }
                TokenType::String(s) => {
                    self.advance();
                    let kind = ExprKind::Literal(Literal::String(s.clone()));
                    Ok(Expr::new(t, kind))
                }
                TokenType::True => {
                    self.advance();
                    let kind = ExprKind::Literal(Literal::Bool(true));
                    Ok(Expr::new(t, kind))
                }
                TokenType::False => {
                    self.advance();
                    let kind = ExprKind::Literal(Literal::Bool(false));
                    Ok(Expr::new(t, kind))
                }
                TokenType::Nil => {
                    self.advance();
                    let kind = ExprKind::Literal(Literal::Nil);
                    Ok(Expr::new(t, kind))
                }
                TokenType::LeftParen => {
                    self.advance();
                    let expr = self.parse_expr()?;
                    match self.advance() {
                        Some(Token {
                            ttype: TokenType::RightParen,
                            ..
                        }) => {
                            let kind = ExprKind::Grouping(Box::new(expr));
                            Ok(Expr::new(t, kind))
                        }
                        _ => Err(self.error(ParseErrorKind::ExpectRightParen)),
                    }
                }
                _ => Err(self.error(ParseErrorKind::NoValidExpr)),
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

    fn error(&self, kind: ParseErrorKind) -> ParseError {
        ParseError {
            token: self.peek(),
            source: kind,
        }
    }
}
