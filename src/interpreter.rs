use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::ast::{Expr, Stmt};
use crate::environment::Environment;
use crate::token::{Token, TokenType};
use crate::Reportable;

#[derive(Debug, Clone)]
pub enum LoxValue {
    Boolean(bool),
    Nil,
    Number(f64),
    String(String),
}

fn is_truthy(val: &LoxValue) -> bool {
    match val {
        LoxValue::Boolean(b) => *b,
        LoxValue::Nil => false,
        _ => true,
    }
}

impl PartialEq for LoxValue {
    fn eq(&self, other: &Self) -> bool {
        println!("{} {}", self, other);
        match (self, other) {
            (LoxValue::Nil, LoxValue::Nil) => false,
            (LoxValue::Boolean(a), LoxValue::Boolean(b)) => a == b,
            (LoxValue::Number(a), LoxValue::Number(b)) => a == b,
            (LoxValue::String(a), LoxValue::String(b)) => a == b,
            (_, _) => false,
        }
    }
}

impl TryFrom<&Token> for LoxValue {
    type Error = InterpreterError;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match &value.t {
            TokenType::Nil => Ok(Self::Nil),
            TokenType::True => Ok(Self::Boolean(true)),
            TokenType::False => Ok(Self::Boolean(false)),
            TokenType::Number(v) => Ok(Self::Number(*v)),
            TokenType::String(v) => Ok(Self::String(v.to_string())),
            _ => Err(InterpreterError::from_token(value, "Unexpected value")),
        }
    }
}

impl fmt::Display for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LoxValue::Number(v) => write!(f, "{}", v),
            LoxValue::String(v) => write!(f, "{}", v),
            LoxValue::Boolean(v) => write!(f, "{}", v),
            LoxValue::Nil => write!(f, "nil"),
        }
    }
}

pub struct InterpreterError {
    line: usize,
    location: String,
    message: String,
}

impl InterpreterError {
    pub fn from_token(token: &Token, message: impl AsRef<str>) -> Self {
        Self {
            line: token.line,
            location: token.lexeme.to_string(),
            message: message.as_ref().to_string(),
        }
    }
}

impl Reportable for InterpreterError {
    fn report(&self) {
        eprintln!(
            "[line {} at {}] Error (Interpreter): {}",
            self.line, self.location, self.message
        );
    }
}

#[derive(Debug)]
pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            environment: Rc::new(RefCell::new(Environment::new(None))),
        }
    }

    pub fn interpret(&self, stmts: Vec<Stmt>) -> Result<(), InterpreterError> {
        for stmt in stmts {
            self.execute(stmt, Rc::clone(&self.environment))?;
        }

        Ok(())
    }

    fn execute(
        &self,
        stmt: Stmt,
        environment: Rc<RefCell<Environment>>,
    ) -> Result<(), InterpreterError> {
        match stmt {
            Stmt::Block { statements } => {
                let block_environment = Rc::new(RefCell::new(Environment::new(Some(Rc::clone(
                    &environment,
                )))));
                for statement in statements {
                    self.execute(statement, Rc::clone(&block_environment))?;
                }
            }
            Stmt::Expression { expression } => {
                self.evaluate(expression, environment)?;
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if is_truthy(&self.evaluate(condition, Rc::clone(&environment))?) {
                    self.execute(*then_branch, Rc::clone(&environment))?;
                } else if let Some(e) = else_branch {
                    self.execute(*e, Rc::clone(&environment))?;
                }
            }
            Stmt::Print { expression } => {
                let value = self.evaluate(expression, Rc::clone(&environment))?;
                println!("{}", value);
            }
            Stmt::Var { name, initializer } => {
                let mut value = LoxValue::Nil;
                if let Some(expr) = initializer {
                    value = self.evaluate(expr, Rc::clone(&environment))?;
                }

                environment.borrow_mut().define(&name, value);
            }
        };

        Ok(())
    }

    fn evaluate(
        &self,
        expr: Expr,
        environment: Rc<RefCell<Environment>>,
    ) -> Result<LoxValue, InterpreterError> {
        match expr {
            Expr::Assign { name, value } => {
                let v = self.evaluate(*value, Rc::clone(&environment))?;
                if environment.borrow_mut().assign(&name, v.clone()).is_none() {
                    return Err(InterpreterError::from_token(&name, "undefined variable"));
                }
                Ok(v)
            }
            Expr::Binary { left, right, op } => {
                let left_val = self.evaluate(*left, Rc::clone(&environment))?;
                let right_val = self.evaluate(*right, Rc::clone(&environment))?;

                match (&op.t, left_val, right_val) {
                    // subtraction
                    (TokenType::Minus, LoxValue::Number(a), LoxValue::Number(b)) => {
                        Ok(LoxValue::Number(a - b))
                    }
                    (TokenType::Minus, _, _) => Err(InterpreterError::from_token(
                        &op,
                        "Subraction can only be done on numbers",
                    )),

                    // division
                    (TokenType::Slash, LoxValue::Number(a), LoxValue::Number(b)) => {
                        Ok(LoxValue::Number(a / b))
                    }
                    (TokenType::Slash, _, _) => Err(InterpreterError::from_token(
                        &op,
                        "Division can only be done on numbers",
                    )),

                    // multiplication
                    (TokenType::Star, LoxValue::Number(a), LoxValue::Number(b)) => {
                        Ok(LoxValue::Number(a * b))
                    }
                    (TokenType::Star, _, _) => Err(InterpreterError::from_token(
                        &op,
                        "Multiplication can only be done on numbers",
                    )),

                    // addition
                    (TokenType::Plus, LoxValue::Number(a), LoxValue::Number(b)) => {
                        Ok(LoxValue::Number(a + b))
                    }
                    (TokenType::Plus, LoxValue::String(a), LoxValue::String(b)) => {
                        Ok(LoxValue::String(format!("{}{}", a, b)))
                    }
                    (TokenType::Plus, _, _) => Err(InterpreterError::from_token(
                        &op,
                        "The + operator can be used to add two numbers or two strings only",
                    )),

                    // greater than
                    (TokenType::Greater, LoxValue::Number(a), LoxValue::Number(b)) => {
                        Ok(LoxValue::Boolean(a > b))
                    }
                    (TokenType::Greater, _, _) => Err(InterpreterError::from_token(
                        &op,
                        "Greater than comparison can only be done on numbers",
                    )),

                    // greater than or equal
                    (TokenType::GreaterEqual, LoxValue::Number(a), LoxValue::Number(b)) => {
                        Ok(LoxValue::Boolean(a >= b))
                    }
                    (TokenType::GreaterEqual, _, _) => Err(InterpreterError::from_token(
                        &op,
                        "Greater than or equal comparison can only be done on numbers",
                    )),

                    // less than
                    (TokenType::Less, LoxValue::Number(a), LoxValue::Number(b)) => {
                        Ok(LoxValue::Boolean(a < b))
                    }
                    (TokenType::Less, _, _) => Err(InterpreterError::from_token(
                        &op,
                        "Less than comparison can only be done on numbers",
                    )),

                    // less than or equal
                    (TokenType::LessEqual, LoxValue::Number(a), LoxValue::Number(b)) => {
                        Ok(LoxValue::Boolean(a < b))
                    }
                    (TokenType::LessEqual, _, _) => Err(InterpreterError::from_token(
                        &op,
                        "Less than or equal comparison can only be done on numbers",
                    )),

                    // not equal
                    (TokenType::BangEqual, a, b) => Ok(LoxValue::Boolean(a != b)),

                    // equal
                    (TokenType::EqualEqual, a, b) => Ok(LoxValue::Boolean(a == b)),

                    // fall through
                    _ => Err(InterpreterError::from_token(&op, "Unknown binary operator")),
                }
            }
            Expr::Grouping { expression } => self.evaluate(*expression, environment),
            Expr::Literal { value } => (&value).try_into(),
            Expr::Logical { left, op, right } => {
                let left_val = self.evaluate(*left, Rc::clone(&environment))?;
                match op.t {
                    TokenType::Or => {
                        if is_truthy(&left_val) {
                            return Ok(left_val);
                        }
                    }
                    TokenType::And => {
                        if !is_truthy(&left_val) {
                            return Ok(left_val);
                        }
                    }
                    _ => {
                        return Err(InterpreterError::from_token(
                            &op,
                            "Unknown logical operator",
                        ))
                    }
                }

                Ok(self.evaluate(*right, Rc::clone(&environment))?)
            }
            Expr::Unary { op, right } => {
                let right_val = self.evaluate(*right, Rc::clone(&environment))?;
                match (&op.t, right_val) {
                    (TokenType::Minus, LoxValue::Number(n)) => Ok(LoxValue::Number(-1.0 * n)),
                    (TokenType::Minus, _) => Err(InterpreterError::from_token(
                        &op,
                        "non-number with unary minus",
                    )),
                    (TokenType::Bang, v) => Ok(LoxValue::Boolean(is_truthy(&v))),
                    _ => Err(InterpreterError::from_token(&op, "Unknown unary operator")),
                }
            }
            Expr::Variable { name } => match environment.borrow().get(&name) {
                Some(value) => Ok(value),
                None => Err(InterpreterError::from_token(&name, "Unknown variable")),
            },
        }
    }
}
