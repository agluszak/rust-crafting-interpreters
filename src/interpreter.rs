use std::cmp::Ordering;
use std::ops::{Neg, Not};

use crate::expr::{BinaryOperator, Expr, Literal, UnaryOperator};
use crate::interpreter::RuntimeError::{DivideByZero, NumberComparisonError, WrongOperator, WrongType, UndefinedVariable};
use crate::stmt::Stmt;
use crate::value::Value;
use std::collections::HashMap;

#[derive(Debug)]
pub enum RuntimeError {
    WrongType,
    // TODO: Add location info
    NumberComparisonError(f64, f64),
    DivideByZero,
    WrongOperator,
    UndefinedVariable
}

type Result<T> = std::result::Result<T, RuntimeError>;

pub struct Environment {
    values: HashMap<String, Value>
}

impl Environment {
    fn new() -> Self {
        Self {
            values: HashMap::new()
        }
    }

    fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    // TODO: should this return a reference?
    fn get(&self, name: String) -> Result<Value> {
        self.values.get(&*name).cloned().ok_or(UndefinedVariable)
    }
}

pub struct Interpreter {
    environment: Environment
}

impl Interpreter {
    pub(crate) fn new() -> Self {
        Self {
            environment: Environment::new(),
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
                let _val = self.evaluate(*expr)?;
                Ok(())
            }
            Stmt::Print(expr) => {
                let val = self.evaluate(*expr)?;
                println!("{:?}", val);
                Ok(())
            }
            Stmt::Var(name, initializer) => {
                let mut value = Value::Nil;
                if let Some(initializer) = initializer {
                    value = self.evaluate(*initializer)?;
                }
                self.environment.define(name, value);
                Ok(())
            }
        }
    }

    fn evaluate(&self, expr: Expr) -> Result<Value> {
        match expr {
            Expr::Binary(left, op, right) => self.evaluate_binary(*left, op, *right),
            Expr::Grouping(expr) => self.evaluate(*expr),
            Expr::Literal(literal) => Self::evaluate_literal(literal),
            Expr::Unary(op, expr) => self.evaluate_unary(op, *expr),
            Expr::Variable(name) => self.environment.get(name),
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

    fn evaluate_unary(&self, op: UnaryOperator, expr: Expr) -> Result<Value> {
        let val = self.evaluate(expr)?;
        match op {
            UnaryOperator::BooleanNegate => require_boolean(&val).map(&bool::not).map(&Value::Boolean),
            UnaryOperator::NumericNegate => require_number(&val).map(&f64::neg).map(&Value::Number),
        }
    }

    fn evaluate_binary(&self, left: Expr, op: BinaryOperator, right: Expr) -> Result<Value> {
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
                require_number(&left)? + require_number(&right)?,
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