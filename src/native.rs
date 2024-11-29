use std::time::{SystemTime, UNIX_EPOCH};

use crate::interpreter::{Callable, Interpreter, RuntimeError, Value};

pub struct NativeClock {}

impl Callable for NativeClock {
    fn name(&self) -> &str {
        "clock"
    }

    fn arity(&self) -> u8 {
        0
    }

    fn call(&self, _interpreter: &mut Interpreter, _args: &[Value]) -> Result<Value, RuntimeError> {
        let time = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
        let time_secs = time.as_secs() as f64;
        Ok(Value::Number(time_secs))
    }
}
