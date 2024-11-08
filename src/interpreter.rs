use std::{collections::HashMap, fmt::Display, mem, slice};

use thiserror::Error;

use crate::{
    expr::{Binary, Expr, ExprKind, Literal, Unary},
    stmt::Stmt,
    token::Token,
};

#[allow(clippy::enum_variant_names)]
#[derive(Clone, Debug, Error, PartialEq)]
pub enum RuntimeErrorKind {
    #[error("Operand must be a number.")]
    OperandNumber,
    #[error("Operands must be numbers.")]
    OperandNumbers,
    #[error("Operands must be two numbers or two strings.")]
    OperandNumbersOrStrings,
    #[error("Undefined variable '{0}'.")]
    UndefinedVariable(String),
}

#[derive(Clone, Debug, Error, PartialEq)]
#[error("{source}\n[line {}]", self.token.line)]
pub struct RuntimeError {
    token: Token,
    source: RuntimeErrorKind,
}

impl RuntimeError {
    fn new(token: Token, kind: RuntimeErrorKind) -> Self {
        Self {
            token,
            source: kind,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}

impl Value {
    fn truthify(&self) -> bool {
        match self {
            Value::Number(_) => true,
            Value::String(_) => true,
            Value::Bool(b) => *b,
            Value::Nil => false,
        }
    }

    fn is_equal(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(left_n), Value::Number(right_n)) => left_n == right_n,
            (Value::String(left_s), Value::String(right_s)) => left_s == right_s,
            (Value::Bool(left_b), Value::Bool(right_b)) => left_b == right_b,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Nil => write!(f, "nil"),
        }
    }
}

struct Environment {
    vars: Vec<HashMap<String, Value>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            vars: vec![HashMap::new()],
        }
    }

    fn define(&mut self, name: String, value: Option<Value>) {
        let value = value.unwrap_or(Value::Nil);
        self.vars
            .last_mut()
            .expect("Globals exist")
            .insert(name, value);
    }

    fn get(&self, name: &str) -> Option<Value> {
        self.vars
            .iter()
            .rev()
            .find_map(|map| map.get(name))
            .cloned()
    }

    fn set(&mut self, name: &str, value: Value) -> Option<Value> {
        self.vars
            .iter_mut()
            .rev()
            .find_map(|map| map.get_mut(name))
            .map(|item| mem::replace(item, value))
    }

    fn push(&mut self) {
        self.vars.push(HashMap::new());
    }

    fn pop(&mut self) {
        self.vars.pop();
    }
}

pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Environment::new(),
        }
    }

    pub fn eval(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        match &expr.kind {
            ExprKind::Literal(lit) => match lit {
                Literal::Number(n) => Ok(Value::Number(*n)),
                Literal::String(s) => Ok(Value::String(s.clone())),
                Literal::Bool(b) => Ok(Value::Bool(*b)),
                Literal::Nil => Ok(Value::Nil),
            },
            ExprKind::Grouping(expr) => self.eval(expr),
            ExprKind::Unary(un) => match un {
                Unary::Negate(expr) => {
                    let expr_value = self.eval(expr)?;
                    match expr_value {
                        Value::Number(n) => Ok(Value::Number(-n)),
                        _ => Err(RuntimeError::new(
                            expr.token.clone(),
                            RuntimeErrorKind::OperandNumber,
                        )),
                    }
                }
                Unary::Not(expr) => {
                    let expr_value = self.eval(expr)?;
                    let truth = expr_value.truthify();
                    Ok(Value::Bool(!truth))
                }
            },
            ExprKind::Binary(bin) => match bin {
                Binary::Add(left, right) => {
                    let left_value = self.eval(left)?;
                    let right_value = self.eval(right)?;

                    match (&left_value, &right_value) {
                        (Value::Number(left_n), Value::Number(right_n)) => {
                            Ok(Value::Number(left_n + right_n))
                        }
                        (Value::String(left_s), Value::String(right_s)) => {
                            Ok(Value::String(format!("{}{}", left_s, right_s)))
                        }
                        _ => Err(RuntimeError::new(
                            left.token.clone(),
                            RuntimeErrorKind::OperandNumbersOrStrings,
                        )),
                    }
                }
                Binary::Sub(left, right) => {
                    let left_value = self.eval(left)?;
                    let right_value = self.eval(right)?;

                    match (&left_value, &right_value) {
                        (Value::Number(left_n), Value::Number(right_n)) => {
                            Ok(Value::Number(left_n - right_n))
                        }
                        _ => Err(RuntimeError::new(
                            left.token.clone(),
                            RuntimeErrorKind::OperandNumbers,
                        )),
                    }
                }
                Binary::Mul(left, right) => {
                    let left_value = self.eval(left)?;
                    let right_value = self.eval(right)?;

                    match (&left_value, &right_value) {
                        (Value::Number(left_n), Value::Number(right_n)) => {
                            Ok(Value::Number(left_n * right_n))
                        }
                        _ => Err(RuntimeError::new(
                            left.token.clone(),
                            RuntimeErrorKind::OperandNumbers,
                        )),
                    }
                }
                Binary::Div(left, right) => {
                    let left_value = self.eval(left)?;
                    let right_value = self.eval(right)?;

                    match (&left_value, &right_value) {
                        (Value::Number(left_n), Value::Number(right_n)) => {
                            Ok(Value::Number(left_n / right_n))
                        }
                        _ => Err(RuntimeError::new(
                            left.token.clone(),
                            RuntimeErrorKind::OperandNumbers,
                        )),
                    }
                }
                Binary::Less(left, right) => {
                    let left_value = self.eval(left)?;
                    let right_value = self.eval(right)?;

                    match (&left_value, &right_value) {
                        (Value::Number(left_n), Value::Number(right_n)) => {
                            Ok(Value::Bool(left_n < right_n))
                        }
                        _ => Err(RuntimeError::new(
                            left.token.clone(),
                            RuntimeErrorKind::OperandNumbers,
                        )),
                    }
                }
                Binary::LessEqual(left, right) => {
                    let left_value = self.eval(left)?;
                    let right_value = self.eval(right)?;

                    match (&left_value, &right_value) {
                        (Value::Number(left_n), Value::Number(right_n)) => {
                            Ok(Value::Bool(left_n <= right_n))
                        }
                        _ => Err(RuntimeError::new(
                            left.token.clone(),
                            RuntimeErrorKind::OperandNumbers,
                        )),
                    }
                }
                Binary::Greater(left, right) => {
                    let left_value = self.eval(left)?;
                    let right_value = self.eval(right)?;

                    match (&left_value, &right_value) {
                        (Value::Number(left_n), Value::Number(right_n)) => {
                            Ok(Value::Bool(left_n > right_n))
                        }
                        _ => Err(RuntimeError::new(
                            left.token.clone(),
                            RuntimeErrorKind::OperandNumbers,
                        )),
                    }
                }
                Binary::GreaterEqual(left, right) => {
                    let left_value = self.eval(left)?;
                    let right_value = self.eval(right)?;

                    match (&left_value, &right_value) {
                        (Value::Number(left_n), Value::Number(right_n)) => {
                            Ok(Value::Bool(left_n >= right_n))
                        }
                        _ => Err(RuntimeError::new(
                            left.token.clone(),
                            RuntimeErrorKind::OperandNumbers,
                        )),
                    }
                }
                Binary::Equal(left, right) => {
                    let left_value = self.eval(left)?;
                    let right_value = self.eval(right)?;

                    let eq = left_value.is_equal(&right_value);
                    Ok(Value::Bool(eq))
                }
                Binary::NotEqual(left, right) => {
                    let left_value = self.eval(left)?;
                    let right_value = self.eval(right)?;

                    let eq = left_value.is_equal(&right_value);
                    Ok(Value::Bool(!eq))
                }
                Binary::Or(left, right) => {
                    let left_value = self.eval(left)?;
                    if left_value.truthify() {
                        Ok(left_value)
                    } else {
                        Ok(self.eval(right)?)
                    }
                }
                Binary::And(left, right) => {
                    let left_value = self.eval(left)?;
                    if !left_value.truthify() {
                        Ok(left_value)
                    } else {
                        Ok(self.eval(right)?)
                    }
                }
            },
            ExprKind::Variable(name) => self.env.get(name).ok_or_else(|| {
                RuntimeError::new(
                    expr.token.clone(),
                    RuntimeErrorKind::UndefinedVariable(name.clone()),
                )
            }),
            ExprKind::Assign(name, value) => {
                let value = self.eval(value)?;
                match self.env.set(name, value.clone()) {
                    Some(_) => Ok(value),
                    None => Err(RuntimeError::new(
                        expr.token.clone(),
                        RuntimeErrorKind::UndefinedVariable(name.clone()),
                    )),
                }
            }
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        for stmt in stmts {
            match stmt {
                Stmt::Expr(expr) => {
                    self.eval(expr)?;
                }
                Stmt::If(cond, true_branch, false_branch) => {
                    let cond_result = self.eval(cond)?.truthify();
                    if cond_result {
                        let tb = slice::from_ref(true_branch.as_ref());
                        self.interpret(tb)?;
                    } else if let Some(fb) = false_branch {
                        let fb = slice::from_ref(fb.as_ref());
                        self.interpret(fb)?;
                    } else {
                        // Do nothing
                    }
                }
                Stmt::Print(expr) => {
                    let value = self.eval(expr)?;
                    println!("{value}");
                }
                Stmt::While(cond, body) => {
                    while self.eval(cond)?.truthify() {
                        let body_slice = slice::from_ref(body.as_ref());
                        self.interpret(body_slice)?;
                    }
                }
                Stmt::VarDecl(name, initializer) => {
                    let value = if let Some(expr) = initializer {
                        Some(self.eval(expr)?)
                    } else {
                        None
                    };
                    self.env.define(name.clone(), value);
                }
                Stmt::Block(stmts) => {
                    self.env.push();
                    self.interpret(stmts)?;
                    self.env.pop();
                }
            }
        }
        Ok(())
    }
}
