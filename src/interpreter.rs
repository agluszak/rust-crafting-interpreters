use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::ops::{Neg, Not};
use std::rc::Rc;

use crate::expr::{BinaryOperator, Expr, ExprType, Literal, UnaryOperator};
use crate::stmt::Stmt;
use crate::value::Value;
use crate::token::Location;

// TODO: impl Error
#[derive(Debug, PartialEq)]
pub enum RuntimeErrorType {
    WrongType,
    // TODO: Add location info
    NumberComparisonError(f64, f64),
    DivideByZero,
    WrongOperator,
    UndefinedVariable(String),
}

#[derive(Debug, PartialEq)]
pub struct RuntimeError {
    error_type: RuntimeErrorType,
    location: Location
}

impl RuntimeError {
    fn new(error_type: RuntimeErrorType,
           location: Location) -> Self {
        Self {
            error_type,
            location
        }
    }
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
                Err(RuntimeError::new(RuntimeErrorType::UndefinedVariable(name), Location::initial()))
                // TODO: Actual location
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
        // TODO: proper location
        maybe_value.ok_or(RuntimeError::new(RuntimeErrorType::UndefinedVariable(name.to_string()), Location::initial()))
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


    pub fn interpret(&mut self, statements: &Vec<Stmt>) -> Result<()> {
        for statement in statements {
            self.execute(statement)?;
        }

        Ok(())
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<Value> {
        match stmt {
            Stmt::Expression(expr) => {
                let val = self.evaluate(expr)?;
                Ok(val)
            }
            Stmt::Print(expr) => {
                let val = self.evaluate(expr)?;
                println!("{:?}", val.clone());
                Ok(val)
            }
            Stmt::Var(name, initializer) => {
                let mut value = Value::Nil;
                if let Some(initializer) = initializer {
                    value = self.evaluate(initializer)?;
                }
                self.environment.borrow_mut().define(name.clone(), value.clone())?;
                Ok(value)
            }
            Stmt::Block(statements) => self.execute_stmts_in_environment(statements, Environment::local(Rc::clone(&self.environment))),
            Stmt::If(condition, then_branch, else_branch) => {
                let condition = self.evaluate(condition)?;
                let mut value = Value::Nil;

                if booleanize(&condition) {
                    value = self.execute(&**then_branch)?;
                } else if let Some(else_branch) = else_branch {
                    value = self.execute(&**else_branch)?;
                }

                Ok(value)
            }
            Stmt::While(condition, stmt) => {
                let mut value = Value::Nil;
                while booleanize(&self.evaluate(condition)?) {
                    value = self.execute(stmt)?;
                }
                Ok(value)
            }
        }
    }

    fn execute_stmts(&mut self, statements: &Vec<Stmt>) -> Result<Value> {
        let mut value = Value::Nil;

        for statement in statements {
            value = self.execute(statement)?
        };

        Ok(value)
    }

    fn execute_stmts_in_environment(&mut self, statements: &Vec<Stmt>, environment: Environment) -> Result<Value> {
        let old = Rc::clone(&self.environment);
        self.environment = Rc::new(RefCell::new(environment));
        let result = self.execute_stmts(statements);
        self.environment = old;
        result
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<Value> {
        match &expr.expr_type {
            ExprType::Binary(left, op, right) => self.evaluate_binary(&**left, *op, &**right),
            ExprType::Grouping(expr) => self.evaluate(&*expr),
            ExprType::Literal(literal) => Self::evaluate_literal(literal),
            ExprType::Unary(op, expr) => self.evaluate_unary(op, expr),
            ExprType::Variable(name) => self.environment.borrow().get(&*name),
            ExprType::Assign(variable, expr) => {
                let value = self.evaluate(&*expr)?;
                self.environment.borrow_mut().assign(variable.clone(), value.clone())?;
                Ok(value)
            }
        }
    }

    fn evaluate_literal(literal: &Literal) -> Result<Value> {
        match literal {
            Literal::String(s) => Ok(Value::String(s.clone())),
            Literal::Number(n) => Ok(Value::Number(*n)),
            Literal::Boolean(b) => Ok(Value::Boolean(*b)),
            Literal::Nil => Ok(Value::Nil),
        }
    }

    fn evaluate_unary(&mut self, op: &UnaryOperator, expr: &Expr) -> Result<Value> {
        let val = self.evaluate(expr)?;
        match op {
            UnaryOperator::BooleanNegate => require_boolean(&val).map(&bool::not).map(&Value::Boolean),
            UnaryOperator::NumericNegate => require_number(&val).map(&f64::neg).map(&Value::Number),
        }
    }

    fn evaluate_binary(&mut self, left: &Expr, op: BinaryOperator, right: &Expr) -> Result<Value> {
        let left = self.evaluate(left)?;
        let left_bool = booleanize(&left);
        // Handle short-circuiting
        // TODO: simplify this
        if let BinaryOperator::Or = op {
            return if left_bool {
                Ok(left)
            } else {
                Ok(self.evaluate(right)?)
            };
        } else if let BinaryOperator::And = op {
            return if !left_bool {
                Ok(left)
            } else {
                Ok(self.evaluate(right)?)
            };
        }

        let right = self.evaluate(right)?;
        match op {
            BinaryOperator::Add => match left {
                Value::Number(left) => Ok(Value::Number(left + require_number(&right)?)),
                Value::String(left) => Ok(Value::String(left + &*require_string(&right)?)),
                // TODO: proper location
                _ => Err(RuntimeError::new(RuntimeErrorType::WrongOperator, Location::initial())),
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
                    // TODO: proper location
                    Err(RuntimeError::new(RuntimeErrorType::DivideByZero, Location::initial()))
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
            BinaryOperator::And => unreachable!(),
            BinaryOperator::Or => unreachable!(),
        }
    }
}

fn require_string(val: &Value) -> Result<String> {
    // TODO: proper location
    val.require_string().ok_or(RuntimeError::new(RuntimeErrorType::WrongType, Location::initial()))
}

fn require_number(val: &Value) -> Result<f64> {
    // TODO: proper location
    val.require_number().ok_or(RuntimeError::new(RuntimeErrorType::WrongType, Location::initial()))
}

fn require_boolean(val: &Value) -> Result<bool> {
    // TODO: proper location
    val.require_boolean().ok_or(RuntimeError::new(RuntimeErrorType::WrongType, Location::initial()))
}

fn booleanize(val: &Value) -> bool {
    match val {
        Value::Nil => false,
        Value::Number(n) => n == &0.0,
        Value::String(s) => s.is_empty(),
        Value::Boolean(b) => *b,
    }
}

#[inline(always)]
fn compare(left: Value, right: Value, f: impl Fn(Ordering) -> bool) -> Result<Value> {
    let ordering = match left {
        Value::Number(left) => {
            let right = require_number(&right)?;
            left.partial_cmp(&right)
                .ok_or(RuntimeError::new(RuntimeErrorType::NumberComparisonError(left, right), Location::initial()))
            // TODO: actual location
        }
        Value::String(left) => Ok(left.cmp(&require_string(&right)?)),
        Value::Boolean(left) => Ok(left.cmp(&require_boolean(&right)?)),
        _ => Err(RuntimeError::new(RuntimeErrorType::WrongType, Location::initial()))
        // TODO: actual location
    }?;

    Ok(Value::Boolean(f(ordering)))
}

#[cfg(test)]
mod tests {
    use crate::interpreter::Interpreter;
    use crate::stmt::Stmt;
    use crate::token::Location;

    #[test]
    fn interpreter_test() {
        use crate::expr::Expr;
        use crate::expr::ExprType::Literal;
        use crate::expr::Literal::Number;

        let mut interpreter = Interpreter::new();
        let set_x = Stmt::Var("x".to_string(), Some(Expr::new(Literal(Number(2.0)), Location::new(3, 4))));

        // assert_eq!(interpreter.evaluate(Expr::Variable("x".to_string())), Err(UndefinedVariable));
        // interpreter.execute(set_x);
        // assert_eq!(interpreter.evaluate(Expr::Variable("x".to_string())), Ok(Value::Number(2.0)));
    }
}

// { var x = 4; print x; }