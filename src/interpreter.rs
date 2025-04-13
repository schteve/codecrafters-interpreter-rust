use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display},
    mem,
    rc::Rc,
    slice,
};

use thiserror::Error;

use crate::{
    expr::{Binary, Binding, Expr, ExprKind, Literal, Unary},
    native,
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
    #[error("Can only call functions and classes.")]
    NotCallable,
    #[error("Expected {0} arguments but got {1}.")]
    WrongArity(u8, u8),
    #[error("Return a value to the caller - nothing is wrong")]
    Return(Option<Value>),
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
    Callable(Rc<dyn Callable>),
}

impl Value {
    fn truthify(&self) -> bool {
        match self {
            Value::Number(_) => true,
            Value::String(_) => true,
            Value::Bool(b) => *b,
            Value::Nil => false,
            Value::Callable(_) => true,
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
            Value::Callable(callable) => write!(f, "<fn {}>", callable.name()),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::Callable(a), Value::Callable(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

pub trait Callable {
    fn name(&self) -> &str;
    fn arity(&self) -> u8;
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, RuntimeError>;
}

impl Debug for dyn Callable {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "<fn {}({})>", self.name(), self.arity())
    }
}

struct Scope {
    parent: Option<Rc<RefCell<Scope>>>,
    vars: HashMap<String, Value>,
}

impl Scope {
    fn new(parent: Option<Rc<RefCell<Scope>>>) -> Self {
        Self {
            parent,
            vars: HashMap::new(),
        }
    }
}

struct Environment {
    curr_scope: Rc<RefCell<Scope>>,
}

impl Environment {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::new(Scope::new(None)));
        Self {
            curr_scope: globals,
        }
    }

    fn define(&mut self, name: String, value: Option<Value>) {
        let value = value.unwrap_or(Value::Nil);
        self.curr_scope.borrow_mut().vars.insert(name, value);
    }

    fn ancestor(&self, depth: Option<u32>) -> Rc<RefCell<Scope>> {
        if let Some(d) = depth {
            // Local
            let mut curr = self.curr_scope.clone();
            for _ in 0..d {
                let ancestor = curr
                    .borrow()
                    .parent
                    .clone()
                    .expect("Environment must be deep enough");
                curr = ancestor;
            }
            curr
        } else {
            // Global
            let mut curr = self.curr_scope.clone();
            loop {
                let Some(ancestor) = curr.borrow().parent.clone() else {
                    break;
                };
                curr = ancestor;
            }
            curr
        }
    }

    fn get(&self, binding: &Binding) -> Option<Value> {
        let scope = self.ancestor(binding.depth);
        let s = scope.borrow();
        s.vars.get(&binding.name).cloned()
    }

    fn set(&mut self, binding: &Binding, value: Value) -> Option<Value> {
        let scope = self.ancestor(binding.depth);
        let mut s = scope.borrow_mut();
        let existing = s.vars.get_mut(&binding.name).expect("Value must be set");
        Some(mem::replace(existing, value))
    }
}

pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut env = Environment::new();

        let nat_fun = native::NativeClock {};
        env.define(
            nat_fun.name().to_owned(),
            Some(Value::Callable(Rc::new(nat_fun))),
        );

        Self { env }
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
            ExprKind::Variable(binding) => self.env.get(binding).ok_or_else(|| {
                RuntimeError::new(
                    expr.token.clone(),
                    RuntimeErrorKind::UndefinedVariable(binding.name.clone()),
                )
            }),
            ExprKind::Assign(binding, value) => {
                let value = self.eval(value)?;
                match self.env.set(binding, value.clone()) {
                    Some(_) => Ok(value),
                    None => Err(RuntimeError::new(
                        expr.token.clone(),
                        RuntimeErrorKind::UndefinedVariable(binding.name.clone()),
                    )),
                }
            }
            ExprKind::Call(callee, arguments) => {
                let callee_value = self.eval(callee)?;
                if let Value::Callable(callable) = callee_value {
                    let args = arguments
                        .iter()
                        .map(|arg| self.eval(arg))
                        .collect::<Result<Vec<_>, _>>()?;

                    let actual_len = args.len() as u8;
                    let expected_len = callable.arity();
                    if actual_len != expected_len {
                        Err(RuntimeError::new(
                            expr.token.clone(),
                            RuntimeErrorKind::WrongArity(expected_len, actual_len),
                        ))
                    } else {
                        let result = callable.call(self, &args);
                        if let Err(RuntimeError {
                            source: RuntimeErrorKind::Return(value),
                            ..
                        }) = result
                        {
                            Ok(value.unwrap_or(Value::Nil))
                        } else {
                            result
                        }
                    }
                } else {
                    Err(RuntimeError::new(
                        expr.token.clone(),
                        RuntimeErrorKind::NotCallable,
                    ))
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
                Stmt::FunDecl(name, params, body) => {
                    let value = Value::Callable(Rc::new(FunCallable {
                        name: name.clone(),
                        params: params.clone(),
                        body: body.clone(),
                        parent_scope: self.env.curr_scope.clone(),
                    }));
                    self.env.define(name.clone(), Some(value));
                }
                Stmt::Block(stmts) => {
                    let curr_scope = self.env.curr_scope.clone();
                    let new_scope = Scope::new(Some(self.env.curr_scope.clone()));
                    self.env.curr_scope = Rc::new(RefCell::new(new_scope));
                    let result = self.interpret(stmts);
                    self.env.curr_scope = curr_scope;
                    result?
                }
                Stmt::Return(keyword, expr) => {
                    let value = if let Some(e) = expr {
                        Some(self.eval(e)?)
                    } else {
                        None
                    };
                    return Err(RuntimeError::new(
                        keyword.clone(),
                        RuntimeErrorKind::Return(value),
                    ));
                }
            }
        }
        Ok(())
    }
}

struct FunCallable {
    name: String,
    params: Vec<String>,
    body: Vec<Stmt>,
    parent_scope: Rc<RefCell<Scope>>,
}

impl Callable for FunCallable {
    fn name(&self) -> &str {
        &self.name
    }

    fn arity(&self) -> u8 {
        self.params.len() as u8
    }

    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, RuntimeError> {
        let curr_scope = interpreter.env.curr_scope.clone();
        let new_scope = Scope::new(Some(self.parent_scope.clone()));
        interpreter.env.curr_scope = Rc::new(RefCell::new(new_scope));

        for (param, arg) in self.params.iter().zip(args) {
            interpreter.env.define(param.clone(), Some(arg.clone()));
        }

        let result = interpreter.interpret(&self.body);

        interpreter.env.curr_scope = curr_scope;

        result.map(|_| Value::Nil)
    }
}
