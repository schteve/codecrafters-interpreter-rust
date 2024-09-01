use std::fmt::Display;

use thiserror::Error;

use crate::parser::{Binary, Expr, Literal, Unary};

#[derive(Clone, Debug, Error, PartialEq)]
pub enum RuntimeError {
    #[error("{0} operation invalid, got {1}")]
    InvalidOperation(String, String),
}

#[derive(Debug)]
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

pub fn eval(expr: &Expr) -> Result<Value, RuntimeError> {
    match expr {
        Expr::Literal(lit) => match lit {
            Literal::Number(n) => Ok(Value::Number(*n)),
            Literal::String(s) => Ok(Value::String(s.clone())),
            Literal::Bool(b) => Ok(Value::Bool(*b)),
            Literal::Nil => Ok(Value::Nil),
        },
        Expr::Grouping(expr) => eval(expr),
        Expr::Unary(un) => match un {
            Unary::Negate(expr) => {
                let expr_value = eval(expr)?;
                match expr_value {
                    Value::Number(n) => Ok(Value::Number(-n)),
                    _ => Err(RuntimeError::InvalidOperation(
                        "Negate".into(),
                        format!("{:?}", expr_value),
                    )),
                }
            }
            Unary::Not(expr) => {
                let expr_value = eval(expr)?;
                let truth = expr_value.truthify();
                Ok(Value::Bool(!truth))
            }
        },
        Expr::Binary(bin) => match bin {
            Binary::Add(left, right) => {
                let left_value = eval(left)?;
                let right_value = eval(right)?;

                match (&left_value, &right_value) {
                    (Value::Number(left_n), Value::Number(right_n)) => {
                        Ok(Value::Number(left_n + right_n))
                    }
                    (Value::String(left_s), Value::String(right_s)) => {
                        Ok(Value::String(format!("{}{}", left_s, right_s)))
                    }
                    _ => Err(RuntimeError::InvalidOperation(
                        "Add".into(),
                        format!("{:?} + {:?}", left_value, right_value),
                    )),
                }
            }
            Binary::Sub(left, right) => {
                let left_value = eval(left)?;
                let right_value = eval(right)?;

                match (&left_value, &right_value) {
                    (Value::Number(left_n), Value::Number(right_n)) => {
                        Ok(Value::Number(left_n - right_n))
                    }
                    _ => Err(RuntimeError::InvalidOperation(
                        "Sub".into(),
                        format!("{:?} - {:?}", left_value, right_value),
                    )),
                }
            }
            Binary::Mul(left, right) => {
                let left_value = eval(left)?;
                let right_value = eval(right)?;

                match (&left_value, &right_value) {
                    (Value::Number(left_n), Value::Number(right_n)) => {
                        Ok(Value::Number(left_n * right_n))
                    }
                    _ => Err(RuntimeError::InvalidOperation(
                        "Mul".into(),
                        format!("{:?} * {:?}", left_value, right_value),
                    )),
                }
            }
            Binary::Div(left, right) => {
                let left_value = eval(left)?;
                let right_value = eval(right)?;

                match (&left_value, &right_value) {
                    (Value::Number(left_n), Value::Number(right_n)) => {
                        Ok(Value::Number(left_n / right_n))
                    }
                    _ => Err(RuntimeError::InvalidOperation(
                        "Div".into(),
                        format!("{:?} / {:?}", left_value, right_value),
                    )),
                }
            }
            Binary::Less(left, right) => {
                let left_value = eval(left)?;
                let right_value = eval(right)?;

                match (&left_value, &right_value) {
                    (Value::Number(left_n), Value::Number(right_n)) => {
                        Ok(Value::Bool(left_n < right_n))
                    }
                    _ => Err(RuntimeError::InvalidOperation(
                        "Less".into(),
                        format!("{:?} </> {:?}", left_value, right_value),
                    )),
                }
            }
            Binary::LessEqual(left, right) => {
                let left_value = eval(left)?;
                let right_value = eval(right)?;

                match (&left_value, &right_value) {
                    (Value::Number(left_n), Value::Number(right_n)) => {
                        Ok(Value::Bool(left_n <= right_n))
                    }
                    _ => Err(RuntimeError::InvalidOperation(
                        "LessEqual".into(),
                        format!("{:?} <=> {:?}", left_value, right_value),
                    )),
                }
            }
            Binary::Greater(left, right) => {
                let left_value = eval(left)?;
                let right_value = eval(right)?;

                match (&left_value, &right_value) {
                    (Value::Number(left_n), Value::Number(right_n)) => {
                        Ok(Value::Bool(left_n > right_n))
                    }
                    _ => Err(RuntimeError::InvalidOperation(
                        "Greater".into(),
                        format!("{:?} > {:?}", left_value, right_value),
                    )),
                }
            }
            Binary::GreaterEqual(left, right) => {
                let left_value = eval(left)?;
                let right_value = eval(right)?;

                match (&left_value, &right_value) {
                    (Value::Number(left_n), Value::Number(right_n)) => {
                        Ok(Value::Bool(left_n >= right_n))
                    }
                    _ => Err(RuntimeError::InvalidOperation(
                        "GreaterEqual".into(),
                        format!("{:?} >= {:?}", left_value, right_value),
                    )),
                }
            }
            Binary::Equal(left, right) => {
                let left_value = eval(left)?;
                let right_value = eval(right)?;

                let eq = left_value.is_equal(&right_value);
                Ok(Value::Bool(eq))
            }
            Binary::NotEqual(left, right) => {
                let left_value = eval(left)?;
                let right_value = eval(right)?;

                let eq = left_value.is_equal(&right_value);
                Ok(Value::Bool(!eq))
            }
        },
    }
}
