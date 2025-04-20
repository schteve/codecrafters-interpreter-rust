use std::{
    fmt::Display,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::interpreter::{Callable, Interpreter, RuntimeError, Value};

#[derive(Clone, Debug)]
pub struct NativeFunction {
    name: &'static str,
    arity: u8,
    func: fn(interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, RuntimeError>,
}

impl Callable for NativeFunction {
    fn name(&self) -> &str {
        self.name
    }

    fn arity(&self) -> u8 {
        self.arity
    }

    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, RuntimeError> {
        (self.func)(interpreter, args)
    }
}

impl Display for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}>", self.name())
    }
}

pub const NATIVE_CLOCK: NativeFunction = NativeFunction {
    name: "clock",
    arity: 0,
    func: |_interpreter: &mut Interpreter, _args: &[Value]| -> Result<Value, RuntimeError> {
        let time = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
        let time_secs = time.as_secs() as f64;
        Ok(Value::Number(time_secs))
    },
};
