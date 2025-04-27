use anyhow::Result;
use thiserror::Error;

use crate::{
    expr::{Binary, Binding, Expr, ExprKind, Literal, Unary},
    stmt::Stmt,
    token::{Token, TokenType},
};

#[derive(Clone, Debug, Eq, Error, PartialEq)]
pub enum ParseErrorKind {
    #[error("No valid expression")]
    NoValidExpr,
    #[error("Expect '(' before expression.")]
    ExpectLeftParen,
    #[error("Expect ')' after expression.")]
    ExpectRightParen,
    #[error("Expect expression.")]
    ExpectExpression,
    #[error("Expect semicolon.")]
    ExpectSemicolon,
    #[error("Expect identifier.")]
    ExpectIdentifier,
    #[error("Expect semicolon or equals.")]
    ExpectSemicolonOrEquals,
    #[error("Invalid assignment target.")]
    InvalidAssignment,
    #[error("Expect left brace before body.")]
    ExpectLeftBrace,
    #[error("Expect right brace.")]
    ExpectRightBrace,
    #[error("Expect Class declaration.")]
    ExpectClass,
    #[error("Expect Var declaration.")]
    ExpectVar,
    #[error("Can't have more than 255 parameters.")]
    TooManyParams,
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

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse_ast(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();
        while let Some(t) = self.peek() {
            if t.ttype == TokenType::Eof {
                break;
            }

            let stmt = self.parse_declaration()?;
            stmts.push(stmt);
        }
        Ok(stmts)
    }

    fn parse_declaration(&mut self) -> Result<Stmt, ParseError> {
        self.peek()
            .ok_or_else(|| self.error(ParseErrorKind::NoValidExpr))
            .and_then(|t| match &t.ttype {
                TokenType::Class => self.parse_class_decl(),
                TokenType::Var => self.parse_var_decl(),
                TokenType::Fun => {
                    self.advance();
                    self.parse_fun_decl()
                }
                _ => self.parse_stmt(),
            })
    }

    fn parse_class_decl(&mut self) -> Result<Stmt, ParseError> {
        self.expect(TokenType::Class, ParseErrorKind::ExpectClass)?;

        let ident = self
            .expect(TokenType::Identifier, ParseErrorKind::ExpectIdentifier)?
            .lexeme;

        self.expect(TokenType::LeftBrace, ParseErrorKind::ExpectLeftBrace)?;

        let mut methods = Vec::new();
        while let Some(t) = self.peek() {
            match t.ttype {
                TokenType::RightBrace => {
                    break;
                }
                TokenType::Eof => break,
                _ => {
                    let function = self.parse_fun_decl()?;
                    methods.push(function);
                }
            }
        }

        self.expect(TokenType::RightBrace, ParseErrorKind::ExpectRightBrace)?;

        Ok(Stmt::ClassDecl(ident, methods))
    }

    fn parse_var_decl(&mut self) -> Result<Stmt, ParseError> {
        self.expect(TokenType::Var, ParseErrorKind::ExpectVar)?;

        let ident = self
            .expect(TokenType::Identifier, ParseErrorKind::ExpectIdentifier)?
            .lexeme;

        let initializer = match self.advance() {
            Some(Token {
                ttype: TokenType::Semicolon,
                ..
            }) => None,
            Some(Token {
                ttype: TokenType::Equal,
                ..
            }) => {
                let expr = self
                    .parse_expr()
                    .map_err(|_| self.error(ParseErrorKind::ExpectExpression))?;
                self.expect(TokenType::Semicolon, ParseErrorKind::ExpectSemicolon)?;
                Some(expr)
            }
            _ => return Err(self.error(ParseErrorKind::ExpectSemicolonOrEquals)),
        };

        Ok(Stmt::VarDecl(ident, initializer))
    }

    fn parse_fun_decl(&mut self) -> Result<Stmt, ParseError> {
        let ident = self
            .expect(TokenType::Identifier, ParseErrorKind::ExpectIdentifier)?
            .lexeme;

        self.expect(TokenType::LeftParen, ParseErrorKind::ExpectLeftParen)?;

        let mut params = Vec::new();
        if !matches!(
            self.peek(),
            Some(Token {
                ttype: TokenType::RightParen,
                ..
            })
        ) {
            loop {
                if params.len() >= 255 {
                    return Err(self.error(ParseErrorKind::TooManyParams));
                }

                let param = self
                    .expect(TokenType::Identifier, ParseErrorKind::ExpectIdentifier)?
                    .lexeme;
                params.push(param);

                if let Some(Token {
                    ttype: TokenType::Comma,
                    ..
                }) = self.peek()
                {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        self.expect(TokenType::RightParen, ParseErrorKind::ExpectRightParen)?;

        let Stmt::Block(body) = self.parse_block()? else {
            unreachable!()
        };

        Ok(Stmt::FunDecl(ident, params, body))
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.peek()
            .ok_or_else(|| self.error(ParseErrorKind::NoValidExpr))
            .and_then(|t| match &t.ttype {
                TokenType::For => {
                    self.advance();

                    self.expect(TokenType::LeftParen, ParseErrorKind::ExpectLeftParen)?;

                    let initializer = match self
                        .peek()
                        .ok_or_else(|| self.error(ParseErrorKind::NoValidExpr))?
                        .ttype
                    {
                        TokenType::Semicolon => {
                            self.advance();

                            None
                        }
                        TokenType::Var => Some(self.parse_var_decl()?),
                        _ => Some(self.parse_expr_stmt()?),
                    };

                    let condition = match self
                        .peek()
                        .ok_or_else(|| self.error(ParseErrorKind::NoValidExpr))?
                        .ttype
                    {
                        TokenType::Semicolon => {
                            self.advance();

                            None
                        }
                        _ => Some(self.parse_expr()?),
                    };
                    let cond_semi_token =
                        self.expect(TokenType::Semicolon, ParseErrorKind::ExpectSemicolon)?;

                    let increment = match self
                        .peek()
                        .ok_or_else(|| self.error(ParseErrorKind::NoValidExpr))?
                        .ttype
                    {
                        TokenType::RightParen => None,
                        _ => Some(self.parse_expr()?),
                    };

                    self.expect(TokenType::RightParen, ParseErrorKind::ExpectRightParen)?;

                    let mut body = self.parse_stmt()?;

                    // Synthesize
                    if let Some(inc) = increment {
                        body = Stmt::Block(vec![body, Stmt::Expr(inc)]);
                    }

                    let cond = condition.unwrap_or(Expr {
                        token: cond_semi_token,
                        kind: ExprKind::Literal(Literal::Bool(true)),
                    });
                    body = Stmt::While(cond, Box::new(body));

                    if let Some(init) = initializer {
                        body = Stmt::Block(vec![init, body]);
                    }

                    Ok(body)
                }
                TokenType::If => {
                    self.advance();

                    self.expect(TokenType::LeftParen, ParseErrorKind::ExpectLeftParen)?;
                    let cond = self.parse_expr()?;
                    self.expect(TokenType::RightParen, ParseErrorKind::ExpectRightParen)?;

                    let true_branch = Box::new(self.parse_stmt()?);
                    let false_branch = if let Some(Token {
                        ttype: TokenType::Else,
                        ..
                    }) = self.peek()
                    {
                        self.advance();

                        Some(Box::new(self.parse_stmt()?))
                    } else {
                        None
                    };

                    Ok(Stmt::If(cond, true_branch, false_branch))
                }
                TokenType::Print => {
                    self.advance();

                    let expr = self.parse_expr()?;
                    self.expect(TokenType::Semicolon, ParseErrorKind::ExpectSemicolon)?;
                    Ok(Stmt::Print(expr))
                }
                TokenType::Return => {
                    let keyword = self.advance().unwrap();
                    let mut expr = None;
                    if !matches!(
                        self.peek(),
                        Some(Token {
                            ttype: TokenType::Semicolon,
                            ..
                        })
                    ) {
                        expr = Some(
                            self.parse_expr()
                                .map_err(|_| self.error(ParseErrorKind::ExpectExpression))?,
                        );
                    }

                    self.expect(TokenType::Semicolon, ParseErrorKind::ExpectSemicolon)?;
                    Ok(Stmt::Return(keyword, expr))
                }
                TokenType::While => {
                    self.advance();

                    self.expect(TokenType::LeftParen, ParseErrorKind::ExpectLeftParen)?;
                    let cond = self.parse_expr()?;
                    self.expect(TokenType::RightParen, ParseErrorKind::ExpectRightParen)?;
                    let body = self.parse_stmt()?;
                    Ok(Stmt::While(cond, Box::new(body)))
                }
                TokenType::LeftBrace => self.parse_block(),
                _ => self.parse_expr_stmt(),
            })
    }

    fn parse_block(&mut self) -> Result<Stmt, ParseError> {
        self.expect(TokenType::LeftBrace, ParseErrorKind::ExpectLeftBrace)?;

        let mut stmts = Vec::new();
        while let Some(t) = self.peek() {
            match t.ttype {
                TokenType::RightBrace => {
                    self.advance();

                    return Ok(Stmt::Block(stmts));
                }
                TokenType::Eof => break,
                _ => {
                    let stmt = self.parse_declaration()?;
                    stmts.push(stmt);
                }
            }
        }
        Err(self.error(ParseErrorKind::ExpectRightBrace))
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.parse_expr()?;
        self.expect(TokenType::Semicolon, ParseErrorKind::ExpectSemicolon)?;
        Ok(Stmt::Expr(expr))
    }

    pub fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.parse_logic_or()?;

        self.peek()
            .ok_or_else(|| self.error(ParseErrorKind::NoValidExpr))
            .and_then(|t| match &t.ttype {
                TokenType::Equal => {
                    self.advance();

                    let value = self.parse_assignment()?;
                    match expr.kind {
                        ExprKind::Variable(binding) => Ok(Expr::new(
                            expr.token,
                            ExprKind::Assign(binding, Box::new(value)),
                        )),
                        ExprKind::Get(obj, property_name) => Ok(Expr::new(
                            expr.token,
                            ExprKind::Set(obj, property_name, Box::new(value)),
                        )),
                        _ => Err(self.error(ParseErrorKind::InvalidAssignment)),
                    }
                }
                _ => Ok(expr),
            })
    }

    fn parse_logic_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_logic_and()?;

        while let Some(Token {
            ttype: TokenType::Or,
            ..
        }) = self.peek()
        {
            let operator = self.advance().unwrap();
            let right = self.parse_logic_and()?;
            let kind = ExprKind::Binary(Binary::Or(Box::new(expr), Box::new(right)));
            expr = Expr::new(operator, kind);
        }

        Ok(expr)
    }

    fn parse_logic_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_equality()?;

        while let Some(Token {
            ttype: TokenType::And,
            ..
        }) = self.peek()
        {
            let operator = self.advance().unwrap();
            let right = self.parse_equality()?;
            let kind = ExprKind::Binary(Binary::And(Box::new(expr), Box::new(right)));
            expr = Expr::new(operator, kind);
        }

        Ok(expr)
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
                _ => self.parse_call(),
            })
    }

    fn parse_call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_primary()?;

        while let Some(next) = self.peek() {
            match next.ttype {
                TokenType::LeftParen => {
                    self.advance();

                    let mut args = Vec::new();

                    if !matches!(
                        self.peek(),
                        Some(Token {
                            ttype: TokenType::RightParen,
                            ..
                        })
                    ) {
                        loop {
                            let arg_expr = self
                                .parse_expr()
                                .map_err(|_| self.error(ParseErrorKind::ExpectExpression))?;
                            args.push(arg_expr);

                            if let Some(Token {
                                ttype: TokenType::Comma,
                                ..
                            }) = self.peek()
                            {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                    }

                    let t = self.expect(TokenType::RightParen, ParseErrorKind::ExpectRightParen)?;
                    let kind = ExprKind::Call(Box::new(expr), args);
                    expr = Expr::new(t, kind);
                }
                TokenType::Dot => {
                    self.advance();

                    let t = self.expect(TokenType::Identifier, ParseErrorKind::ExpectIdentifier)?;
                    let property_name = t.lexeme.clone();
                    let kind = ExprKind::Get(Box::new(expr), property_name);
                    expr = Expr::new(t, kind);
                }
                _ => break,
            }
        }

        Ok(expr)
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
                    self.expect(TokenType::RightParen, ParseErrorKind::ExpectRightParen)?;
                    let kind = ExprKind::Grouping(Box::new(expr));
                    Ok(Expr::new(t, kind))
                }
                TokenType::Identifier => {
                    self.advance();

                    let name = t.lexeme.clone();
                    let binding = Binding { name, depth: None };
                    let kind = ExprKind::Variable(binding);
                    Ok(Expr::new(t, kind))
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

    fn expect(&mut self, ttype: TokenType, err_kind: ParseErrorKind) -> Result<Token, ParseError> {
        self.advance()
            .filter(|t| t.ttype == ttype)
            .ok_or(self.error(err_kind))
    }

    fn error(&self, kind: ParseErrorKind) -> ParseError {
        ParseError {
            token: self.peek(),
            source: kind,
        }
    }
}
