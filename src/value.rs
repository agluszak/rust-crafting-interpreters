use std::fmt::Debug;
use std::rc::Rc;
use crate::interpreter::{Interpreter, RuntimeError};
use crate::stmt::Stmt;

#[derive(Clone)]
pub struct Callable {
    pub name: String,
    pub arity: usize,
    pub func: Rc<dyn Fn(Vec<Value>, &mut Interpreter) -> Result<Value, RuntimeError>>,
}

// TODO
#[allow(clippy::ptr_eq)]
impl PartialEq for Callable {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.arity == other.arity && &self.func as *const _ == &other.func as *const _
    }
}

impl Debug for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Callable {
    pub fn from_definition(name: String, params: Vec<String>, body: Vec<Stmt>) -> Self {
        Self {
            name,
            arity: params.len(),
            func: Rc::new(move |args, interpreter: &mut Interpreter| {
                for (param, arg) in params.iter().zip(args) {
                    interpreter.define(param.clone(), arg);
                }
                interpreter.interpret(&body).map(|_| Value::Nil)
            }),
        }
    }

    pub fn new_native(name: String, arity: usize, func: Rc<dyn Fn(Vec<Value>, &mut Interpreter) -> Result<Value, RuntimeError>>) -> Self {
        Self {
            name,
            arity,
            func,
        }
    }

    pub fn call(&self, args: Vec<Value>, interpreter: &mut Interpreter) -> Result<Value, RuntimeError> {
        (self.func)(args, interpreter)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Number(f64),
    String(String),
    Boolean(bool),
    Callable(Callable),
}

impl Value {
    pub fn lox_string(&self) -> String {
        match self {
            Value::Nil => "nil".to_string(),
            Value::Number(n) => n.to_string(),
            Value::String(s) => format!("\"{}\"", s.clone()),
            Value::Boolean(b) => b.to_string(),
            Value::Callable(c) => format!("<function {}({})>", c.name, c.arity),
        }
    }

    pub fn require_boolean(&self) -> Option<bool> {
        if let Value::Boolean(b) = self {
            Some(*b)
        } else {
            None
        }
    }

    pub fn require_number(&self) -> Option<f64> {
        if let Value::Number(n) = self {
            Some(*n)
        } else {
            None
        }
    }

    pub fn require_string(&self) -> Option<String> {
        if let Value::String(s) = self {
            Some(s.clone())
        } else {
            None
        }
    }

    pub fn require_nil(&self) -> Option<()> {
        if let Value::Nil = self {
            Some(())
        } else {
            None
        }
    }

    pub fn require_callable(&self) -> Option<Callable> {
        if let Value::Callable(c) = self {
            Some(c.clone())
        } else {
            None
        }
    }
}
