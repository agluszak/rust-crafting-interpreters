use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::ops::{Neg, Not};
use std::rc::Rc;

use crate::expr::{BinaryOperator, Expr, Literal, UnaryOperator};
use crate::interpreter::RuntimeError::{DivideByZero, NumberComparisonError, UndefinedVariable, WrongOperator, WrongType};
use crate::stmt::Stmt;
use crate::value::Value;

#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    WrongType,
    // TODO: Add location info
    NumberComparisonError(f64, f64),
    DivideByZero,
    WrongOperator,
    UndefinedVariable,
}

type Result<T> = std::result::Result<T, RuntimeError>;

pub struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Value>,
}

impl Environment {
    fn global() -> Self {
        Self {
            enclosing: None,
            values: HashMap::new(),
        }
    }

    fn local(enclosing: Rc<RefCell<Environment>>) -> Self {
        Self {
            enclosing: Some(enclosing),
            values: HashMap::new(),
        }
    }

    fn define(&mut self, name: String, value: Value) -> Result<()> {
        self.values.insert(name, value);
        Ok(())
    }

    fn is_defined(&self, name: &str) -> bool {
        self.values.contains_key(name)
    }

    fn assign(&mut self, name: String, value: Value) -> Result<()> {
        if self.is_defined(&*name) {
            self.values.insert(name, value);
            Ok(())
        } else {
            if let Some(enclosing) = &self.enclosing {
                enclosing.borrow_mut().assign(name, value)
            } else {
                Err(RuntimeError::UndefinedVariable)
            }
        }
    }

    // TODO: should this return a reference?
    fn get(&self, name: &str) -> Result<Value> {
        let maybe_value = self.values.get(name).cloned();
        let maybe_value =
            if let Some(enclosing) = &self.enclosing {
                maybe_value.or(enclosing.borrow().get(name).ok())
            } else {
                maybe_value
            };
        maybe_value.ok_or(UndefinedVariable)
    }
}

pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            environment: Rc::new(RefCell::new(Environment::global())),
        }
    }


    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<()> {
        for statement in statements {
            self.execute(statement)?;
        }

        Ok(())
    }

    fn execute(&mut self, stmt: Stmt) -> Result<()> {
        match stmt {
            Stmt::Expression(expr) => {
                let _val = self.evaluate(expr)?;
                Ok(())
            }
            Stmt::Print(expr) => {
                let val = self.evaluate(expr)?;
                println!("{:?}", val);
                Ok(())
            }
            Stmt::Var(name, initializer) => {
                let mut value = Value::Nil;
                if let Some(initializer) = initializer {
                    value = self.evaluate(initializer)?;
                }
                self.environment.borrow_mut().define(name, value)?;
                Ok(())
            }
            Stmt::Block(statements) => self.execute_stmts_in_environment(statements, Environment::local(Rc::clone(&self.environment)))
        }
    }

    fn execute_stmts(&mut self, statements: Vec<Stmt>) -> Result<()> {
        for statement in statements {
            self.execute(statement)?
        }

        Ok(())
    }

    fn execute_stmts_in_environment(&mut self, statements: Vec<Stmt>, environment: Environment) -> Result<()> {
        let old = Rc::clone(&self.environment);
        self.environment = Rc::new(RefCell::new(environment));
        let result = self.execute_stmts(statements);
        self.environment = old;
        result
    }

    fn evaluate(&mut self, expr: Expr) -> Result<Value> {
        match expr {
            Expr::Binary(left, op, right) => self.evaluate_binary(*left, op, *right),
            Expr::Grouping(expr) => self.evaluate(*expr),
            Expr::Literal(literal) => Self::evaluate_literal(literal),
            Expr::Unary(op, expr) => self.evaluate_unary(op, *expr),
            Expr::Variable(name) => self.environment.borrow().get(&*name),
            Expr::Assign(variable, expr) => {
                let value = self.evaluate(*expr)?;
                self.environment.borrow_mut().assign(variable, value.clone())?;
                Ok(value)
            }
        }
    }

    fn evaluate_literal(literal: Literal) -> Result<Value> {
        match literal {
            Literal::String(s) => Ok(Value::String(s)),
            Literal::Number(n) => Ok(Value::Number(n)),
            Literal::Boolean(b) => Ok(Value::Boolean(b)),
            Literal::Nil => Ok(Value::Nil),
        }
    }

    fn evaluate_unary(&mut self, op: UnaryOperator, expr: Expr) -> Result<Value> {
        let val = self.evaluate(expr)?;
        match op {
            UnaryOperator::BooleanNegate => require_boolean(&val).map(&bool::not).map(&Value::Boolean),
            UnaryOperator::NumericNegate => require_number(&val).map(&f64::neg).map(&Value::Number),
        }
    }

    fn evaluate_binary(&mut self, left: Expr, op: BinaryOperator, right: Expr) -> Result<Value> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;
        match op {
            BinaryOperator::Add => match left {
                Value::Number(left) => Ok(Value::Number(left + require_number(&right)?)),
                Value::String(left) => Ok(Value::String(left + &*require_string(&right)?)),
                _ => Err(WrongOperator),
            },

            BinaryOperator::Subtract => Ok(Value::Number(
                require_number(&left)? + require_number(&right)?,
            )),
            BinaryOperator::Multiply => Ok(Value::Number(
                require_number(&left)? * require_number(&right)?,
            )),
            BinaryOperator::Divide => {
                let left = require_number(&left)?;
                let right = require_number(&right)?;
                if right == 0.0 {
                    Err(DivideByZero)
                } else {
                    Ok(Value::Number(left / right))
                }
            }
            BinaryOperator::Less => compare(left, right, &Ordering::is_lt),
            BinaryOperator::LessEqual => compare(left, right, &Ordering::is_le),
            BinaryOperator::Greater => compare(left, right, &Ordering::is_gt),
            BinaryOperator::GreaterEqual => compare(left, right, &Ordering::is_ge),
            BinaryOperator::Equal => compare(left, right, &Ordering::is_eq),
            BinaryOperator::NotEqual => compare(left, right, &Ordering::is_ne),
            BinaryOperator::And => Ok(Value::Boolean(booleanize(left) && booleanize(right))),
            BinaryOperator::Or => Ok(Value::Boolean(booleanize(left) || booleanize(right))),
        }
    }
}

fn require_string(val: &Value) -> Result<String> {
    val.require_string().ok_or(WrongType)
}

fn require_number(val: &Value) -> Result<f64> {
    val.require_number().ok_or(WrongType)
}

fn require_boolean(val: &Value) -> Result<bool> {
    val.require_boolean().ok_or(WrongType)
}

fn booleanize(val: Value) -> bool {
    match val {
        Value::Nil => false,
        Value::Number(n) => n == 0.0,
        Value::String(s) => s.is_empty(),
        Value::Boolean(b) => b,
    }
}

#[inline(always)]
fn compare(left: Value, right: Value, f: impl Fn(Ordering) -> bool) -> Result<Value> {
    let ordering = match left {
        Value::Number(left) => {
            let right = require_number(&right)?;
            left.partial_cmp(&right)
                .ok_or(NumberComparisonError(left, right))
        }
        Value::String(left) => Ok(left.cmp(&require_string(&right)?)),
        Value::Boolean(left) => Ok(left.cmp(&require_boolean(&right)?)),
        _ => Err(WrongType), // TODO: more specific error
    }?;

    Ok(Value::Boolean(f(ordering)))
}

#[cfg(test)]
mod tests {
    use crate::interpreter::Interpreter;
    use crate::stmt::Stmt;

    #[test]
    fn interpreter_test() {
        use crate::expr::Expr::Literal;
        use crate::expr::Literal::Number;

        let mut interpreter = Interpreter::new();
        let set_x = Stmt::Var("x".to_string(), Some(Literal(Number(2.0))));

        // assert_eq!(interpreter.evaluate(Expr::Variable("x".to_string())), Err(UndefinedVariable));
        // interpreter.execute(set_x);
        // assert_eq!(interpreter.evaluate(Expr::Variable("x".to_string())), Ok(Value::Number(2.0)));
    }
}

// { var x = 4; print x; }