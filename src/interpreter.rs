use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display},
    mem, ptr,
    rc::Rc,
    slice,
};

use thiserror::Error;

use crate::{
    expr::{Binary, Binding, Expr, ExprKind, Literal, Unary},
    native::{self, NativeFunction},
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
    #[error("Only instances have properties.")]
    NotAnInstance,
    #[error("Undefined property {0}.")]
    UndefinedProperty(String),
    #[error("Superclass must be a class.")]
    SuperNotClass,
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
    NativeFunction(NativeFunction),
    Function(Rc<Function>),
    Class(Rc<Class>),
    Instance(Rc<RefCell<Instance>>),
}

impl Value {
    fn truthify(&self) -> bool {
        match self {
            Value::Number(_) => true,
            Value::String(_) => true,
            Value::Bool(b) => *b,
            Value::Nil => false,
            Value::NativeFunction(_) => true,
            Value::Function(_) => true,
            Value::Class(_) => true,
            Value::Instance(_) => true,
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
            Value::NativeFunction(nf) => write!(f, "{nf}"),
            Value::Function(func) => write!(f, "{func}"),
            Value::Class(c) => write!(f, "{c}"),
            Value::Instance(inst) => write!(f, "{}", inst.borrow()),
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
            (Value::NativeFunction(a), Value::NativeFunction(b)) => {
                ptr::addr_eq(ptr::addr_of!(a), ptr::addr_of!(b))
            }
            (Value::Function(a), Value::Function(b)) => Rc::ptr_eq(a, b),
            (Value::Class(a), Value::Class(b)) => Rc::ptr_eq(a, b),
            (Value::Instance(a), Value::Instance(b)) => {
                ptr::addr_eq(ptr::addr_of!(a), ptr::addr_of!(b))
            }
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

#[derive(Clone, Debug)]
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

    fn define(&mut self, name: String, value: Value) {
        self.vars.insert(name, value);
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
        self.curr_scope.borrow_mut().define(name, value);
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

        let nat_fun = native::NATIVE_CLOCK;
        env.define(
            nat_fun.name().to_owned(),
            Some(Value::NativeFunction(nat_fun)),
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

                    let eq = left_value == right_value;
                    Ok(Value::Bool(eq))
                }
                Binary::NotEqual(left, right) => {
                    let left_value = self.eval(left)?;
                    let right_value = self.eval(right)?;

                    let neq = left_value != right_value;
                    Ok(Value::Bool(neq))
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
                let callable = match &callee_value {
                    Value::NativeFunction(nat_func) => Some(nat_func as &dyn Callable),
                    Value::Function(function) => Some(function.as_ref() as &dyn Callable),
                    Value::Class(class) => Some(class.as_ref() as &dyn Callable),
                    _ => None,
                };
                if let Some(callable) = callable {
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
            ExprKind::Get(obj, property_name) => {
                let obj_value = self.eval(obj)?;
                if let Value::Instance(inst) = obj_value {
                    inst.get(property_name)
                } else {
                    Err(RuntimeError::new(
                        obj.token.clone(),
                        RuntimeErrorKind::NotAnInstance,
                    ))
                }
            }
            ExprKind::Set(obj, property_name, value) => {
                let obj_value = self.eval(obj)?;
                if let Value::Instance(mut inst) = obj_value {
                    let value_value = self.eval(value)?;
                    inst.set(property_name.clone(), value_value.clone());
                    Ok(value_value)
                } else {
                    Err(RuntimeError::new(
                        obj.token.clone(),
                        RuntimeErrorKind::NotAnInstance,
                    ))
                }
            }
            ExprKind::This(binding) => self.env.get(binding).ok_or_else(|| {
                RuntimeError::new(
                    expr.token.clone(),
                    RuntimeErrorKind::UndefinedVariable(binding.name.clone()),
                )
            }),
            ExprKind::Super(binding, method) => {
                // Find super from same location as method's binding
                let Value::Class(superclass) = self.env.get(binding).ok_or_else(|| {
                    RuntimeError::new(
                        expr.token.clone(),
                        RuntimeErrorKind::UndefinedVariable(binding.name.clone()),
                    )
                })?
                else {
                    panic!("Found non-class value in superclass");
                };

                // Find this from one level higher than super
                let this_binding = Binding {
                    name: String::from("this"),
                    depth: binding.depth.map(|d| d - 1),
                };
                let Value::Instance(this) = self.env.get(&this_binding).ok_or_else(|| {
                    RuntimeError::new(
                        expr.token.clone(),
                        RuntimeErrorKind::UndefinedVariable(binding.name.clone()),
                    )
                })?
                else {
                    panic!("Found non-instance object in this");
                };

                // Find method from superclass
                let method = superclass.find_method(method).ok_or_else(|| {
                    RuntimeError::new(
                        Token::empty(),
                        RuntimeErrorKind::UndefinedProperty(binding.name.clone()),
                    )
                })?;

                let function = method.bind(&this);
                Ok(Value::Function(Rc::new(function)))
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
                Stmt::ClassDecl(name, methods, superclass) => {
                    let rc_superclass = if let Some(sup) = superclass {
                        if let Value::Class(rc_class) = self.eval(sup)? {
                            Some(rc_class)
                        } else {
                            return Err(RuntimeError::new(
                                sup.token.clone(),
                                RuntimeErrorKind::SuperNotClass,
                            ));
                        }
                    } else {
                        None
                    };

                    if let Some(sup) = superclass {
                        let super_value = self.eval(sup)?;

                        let curr_scope = self.env.curr_scope.clone();
                        let new_scope = Scope::new(Some(curr_scope));
                        self.env.curr_scope = Rc::new(RefCell::new(new_scope));
                        self.env.define(String::from("super"), Some(super_value));
                    }

                    let mut method_map = HashMap::new();
                    for method in methods {
                        let Stmt::FunDecl(name, params, body) = method else {
                            panic!("Found non-function declaration in method list");
                        };

                        let f = Function {
                            name: name.clone(),
                            params: params.clone(),
                            body: body.clone(),
                            parent_scope: self.env.curr_scope.clone(),
                            is_initializer: name == "init",
                        };
                        method_map.insert(name.clone(), f);
                    }

                    if superclass.is_some() {
                        let parent = self
                            .env
                            .curr_scope
                            .borrow()
                            .parent
                            .clone()
                            .expect("Environment must be deep enough");
                        self.env.curr_scope = parent;
                    }

                    let class = Class {
                        name: name.clone(),
                        methods: method_map,
                        superclass: rc_superclass,
                    };
                    let value = Value::Class(Rc::new(class));
                    self.env.define(name.clone(), Some(value));
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
                    let value = Value::Function(Rc::new(Function {
                        name: name.clone(),
                        params: params.clone(),
                        body: body.clone(),
                        parent_scope: self.env.curr_scope.clone(),
                        is_initializer: false,
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

#[derive(Clone, Debug)]
pub struct Function {
    name: String,
    params: Vec<String>,
    body: Vec<Stmt>,
    parent_scope: Rc<RefCell<Scope>>,
    is_initializer: bool,
}

impl Function {
    fn bind(&self, instance: &Rc<RefCell<Instance>>) -> Self {
        let mut this_scope = Scope::new(Some(self.parent_scope.clone()));
        let this = Value::Instance(instance.clone());
        this_scope.define(String::from("this"), this);

        let mut function = self.clone();
        function.parent_scope = Rc::new(RefCell::new(this_scope));
        function
    }
}

impl Callable for Function {
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

        match result {
            Ok(()) => {
                if self.is_initializer {
                    let s = self.parent_scope.borrow();
                    let this = s.vars.get("this").expect("'this' must exist'").clone();
                    Ok(this)
                } else {
                    Ok(Value::Nil)
                    // result.map(|_| Value::Nil)
                }
            }
            Err(mut err) => {
                if self.is_initializer && matches!(err.source, RuntimeErrorKind::Return(_)) {
                    let s = self.parent_scope.borrow();
                    let this = s.vars.get("this").expect("'this' must exist'").clone();
                    err.source = RuntimeErrorKind::Return(Some(this));
                }
                Err(err)
            }
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}>", self.name())
    }
}

#[derive(Clone, Debug)]
pub struct Class {
    name: String,
    methods: HashMap<String, Function>,
    superclass: Option<Rc<Class>>,
}

impl Class {
    fn find_method(&self, name: &str) -> Option<&Function> {
        self.methods.get(name).or_else(|| {
            if let Some(sup) = &self.superclass {
                sup.find_method(name)
            } else {
                None
            }
        })
    }
}

impl Callable for Class {
    fn name(&self) -> &str {
        &self.name
    }

    fn arity(&self) -> u8 {
        if let Some(init) = self.find_method("init") {
            init.arity()
        } else {
            0
        }
    }

    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, RuntimeError> {
        let inst = Instance::new(self.clone());
        let inst = Rc::new(RefCell::new(inst));

        if let Some(init) = self.find_method("init") {
            init.bind(&inst).call(interpreter, args)?;
        }

        Ok(Value::Instance(inst))
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Clone, Debug)]
pub struct Instance {
    class: Class,
    fields: HashMap<String, Value>,
}

impl Instance {
    fn new(class: Class) -> Self {
        Self {
            class,
            fields: HashMap::new(),
        }
    }
}

// This is goofy but it lets us refer back to the correct instance without
// passing it in as a parameter. Could also use a global registry of instances
// and use UUID to look it up.
trait HasProperty {
    fn get(&self, property_name: &str) -> Result<Value, RuntimeError>;
    fn set(&mut self, property_name: String, value: Value);
}

impl HasProperty for Rc<RefCell<Instance>> {
    fn get(&self, property_name: &str) -> Result<Value, RuntimeError> {
        let inst = self.borrow();
        if let Some(field) = inst.fields.get(property_name) {
            Ok(field.clone())
        } else if let Some(method) = inst.class.find_method(property_name) {
            let function = method.bind(self);
            Ok(Value::Function(Rc::new(function)))
        } else {
            Err(RuntimeError::new(
                Token::empty(),
                RuntimeErrorKind::UndefinedProperty(property_name.to_owned()),
            ))
        }
    }

    fn set(&mut self, property_name: String, value: Value) {
        let mut inst = self.borrow_mut();
        inst.fields.insert(property_name, value);
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} instance", self.class.name)
    }
}
