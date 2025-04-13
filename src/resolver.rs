use std::collections::HashMap;

use thiserror::Error;

use crate::{
    expr::{Binary, Expr, ExprKind, Unary},
    stmt::Stmt,
    token::{Token, TokenType},
};

#[derive(Clone, Debug, Error, PartialEq)]
pub enum ResolverErrorKind {
    #[error("Can't read local variable '{0}' in its own initializer.")]
    VariableSelfAccess(String),
    #[error("Already a variable named '{0}' in this scope.")]
    VariableRedeclaration(String),
}

#[derive(Clone, Debug, Error, PartialEq)]
#[error("{source}\n[line {}]", self.token.line)]
pub struct ResolverError {
    token: Token,
    source: ResolverErrorKind,
}

impl ResolverError {
    fn new(token: Token, kind: ResolverErrorKind) -> Self {
        Self {
            token,
            source: kind,
        }
    }
}

struct Scope {
    idents: HashMap<String, bool>,
}

impl Scope {
    fn new() -> Self {
        Self {
            idents: HashMap::new(),
        }
    }
}

pub struct Resolver {
    scopes: Vec<Scope>,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new()],
        }
    }

    pub fn resolve(&mut self, stmts: &mut [Stmt]) -> Result<(), ResolverError> {
        for stmt in stmts.iter_mut() {
            self.resolve_stmt(stmt)?;
        }
        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &mut Stmt) -> Result<(), ResolverError> {
        match stmt {
            Stmt::Expr(expr) => {
                self.resolve_expr(expr)?;
            }
            Stmt::If(cond, true_branch, false_branch) => {
                self.resolve_expr(cond)?;
                self.resolve_stmt(true_branch.as_mut())?;
                if let Some(false_branch) = false_branch {
                    self.resolve_stmt(false_branch.as_mut())?;
                }
            }
            Stmt::Print(expr) => {
                self.resolve_expr(expr)?;
            }
            Stmt::While(cond, body) => {
                self.resolve_expr(cond)?;
                self.resolve_stmt(body.as_mut())?;
            }
            Stmt::VarDecl(name, initializer) => {
                self.declare(name.clone())?;
                if let Some(init_expr) = initializer {
                    self.resolve_expr(init_expr)?;
                }
                self.define(name.clone());
            }
            Stmt::FunDecl(name, params, body) => {
                self.declare(name.clone())?;
                self.define(name.clone());
                self.resolve_fn(params, body)?;
            }
            Stmt::Block(stmts) => {
                self.scope_begin();
                self.resolve(stmts)?;
                self.scope_end();
            }
            Stmt::Return(_keyword, expr) => {
                if let Some(e) = expr {
                    self.resolve_expr(e)?;
                }
            }
        }
        Ok(())
    }

    pub fn resolve_expr(&mut self, expr: &mut Expr) -> Result<(), ResolverError> {
        match &mut expr.kind {
            ExprKind::Literal(_lit) => (), // Nothing to do
            ExprKind::Grouping(expr) => {
                self.resolve_expr(expr)?;
            }
            ExprKind::Unary(un) => {
                let expr = match un {
                    Unary::Negate(expr) => expr,
                    Unary::Not(expr) => expr,
                };
                self.resolve_expr(expr.as_mut())?;
            }
            ExprKind::Binary(bin) => {
                let (left, right) = match bin {
                    Binary::Add(left, right) => (left, right),
                    Binary::Sub(left, right) => (left, right),
                    Binary::Mul(left, right) => (left, right),
                    Binary::Div(left, right) => (left, right),
                    Binary::Less(left, right) => (left, right),
                    Binary::LessEqual(left, right) => (left, right),
                    Binary::Greater(left, right) => (left, right),
                    Binary::GreaterEqual(left, right) => (left, right),
                    Binary::Equal(left, right) => (left, right),
                    Binary::NotEqual(left, right) => (left, right),
                    Binary::Or(left, right) => (left, right),
                    Binary::And(left, right) => (left, right),
                };

                self.resolve_expr(left.as_mut())?;
                self.resolve_expr(right.as_mut())?;
            }
            ExprKind::Variable(binding) => {
                if self.scopes.len() > 1 // Only error on local scope
                    && self
                        .scopes
                        .last()
                        .expect("Some scope must exist")
                        .idents
                        .get(&binding.name)
                        == Some(&false)
                {
                    return Err(ResolverError::new(
                        expr.token.clone(),
                        ResolverErrorKind::VariableSelfAccess(binding.name.clone()),
                    ));
                }

                binding.depth = self.resolve_local(&binding.name);
            }
            ExprKind::Assign(binding, expr) => {
                self.resolve_expr(expr)?;
                binding.depth = self.resolve_local(&binding.name);
            }
            ExprKind::Call(callee, arguments) => {
                self.resolve_expr(callee.as_mut())?;
                for arg in arguments.iter_mut() {
                    self.resolve_expr(arg)?;
                }
            }
        }
        Ok(())
    }

    fn resolve_local(&mut self, name: &str) -> Option<u32> {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.idents.contains_key(name) {
                return Some(i as u32);
            }
        }
        None
    }

    fn resolve_fn(&mut self, params: &[String], body: &mut [Stmt]) -> Result<(), ResolverError> {
        self.scope_begin();
        for param in params {
            self.declare(param.clone())?;
            self.define(param.clone());
        }
        self.resolve(body)?;
        self.scope_end();

        Ok(())
    }

    fn scope_begin(&mut self) {
        let scope = Scope::new();
        self.scopes.push(scope);
    }

    fn scope_end(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: String) -> Result<(), ResolverError> {
        let idents = &mut self
            .scopes
            .last_mut()
            .expect("Some scope must exist")
            .idents;

        let prev = idents.insert(name.clone(), false);
        if self.scopes.len() > 1 && prev.is_some() {
            // Only error on local scope
            return Err(ResolverError::new(
                Token {
                    ttype: TokenType::Eof,
                    lexeme: String::new(),
                    line: 0,
                }, // There should be an actual token for this, but not all statements have one. Refactor?
                ResolverErrorKind::VariableRedeclaration(name.clone()),
            ));
        }
        Ok(())
    }

    fn define(&mut self, name: String) {
        let idents = &mut self
            .scopes
            .last_mut()
            .expect("Some scope must exist")
            .idents;
        let prev = idents.insert(name, true);
        assert!(prev.is_some(), "Declaration must precede definition");
    }
}
