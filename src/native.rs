use std::time::{SystemTime, UNIX_EPOCH};

use crate::interpreter::{Callable, Interpreter, Value};

pub struct NativeClock {}

impl Callable for NativeClock {
    fn arity(&self) -> u8 {
        0
    }

    fn call(&self, _interpreter: &mut Interpreter, _args: &[Value]) -> Value {
        let time = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
        Value::Number(time.as_secs() as f64)
    }
}
