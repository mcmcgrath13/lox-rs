use std::fmt;

use crate::ast::Expr;
use crate::token::{Token, TokenType};
use crate::Reportable;

pub enum LoxValue {
    Boolean(bool),
    Nil,
    Number(f64),
    String(String),
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

impl TryFrom<&Token<'_>> for LoxValue {
    type Error = InterpreterError;

    fn try_from(value: &Token<'_>) -> Result<Self, Self::Error> {
        match value.t {
            TokenType::Nil => Ok(Self::Nil),
            TokenType::True => Ok(Self::Boolean(true)),
            TokenType::False => Ok(Self::Boolean(false)),
            TokenType::Number(v) => Ok(Self::Number(v)),
            TokenType::String(v) => Ok(Self::String(v.to_string())),
            _ => Err(InterpreterError::from_token(
                value,
                "Unexpected value".to_string(),
            )),
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
    pub fn from_token(token: &Token<'_>, message: String) -> Self {
        Self {
            line: token.line,
            location: token.lexeme.to_string(),
            message,
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
pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn interpret(&self, expr: Expr) -> Result<LoxValue, InterpreterError> {
        match expr {
            Expr::Literal { value } => (&value).try_into(),
            Expr::Unary { op, right } => {
                let right_val = self.interpret(*right)?;
                match (op.t, right_val) {
                    (TokenType::Minus, LoxValue::Number(n)) => Ok(LoxValue::Number(-1.0 * n)),
                    (TokenType::Minus, _) => Err(InterpreterError::from_token(
                        &op,
                        "non-number with unary minus".to_string(),
                    )),
                    (TokenType::Bang, LoxValue::Boolean(b)) => Ok(LoxValue::Boolean(!b)),
                    (TokenType::Bang, LoxValue::Nil) => Ok(LoxValue::Boolean(true)), // nil is falsy
                    (TokenType::Bang, _) => Err(InterpreterError::from_token(
                        &op,
                        "non-boolean with unary exclamation".to_string(),
                    )),
                    _ => Err(InterpreterError::from_token(
                        &op,
                        "Unknown unary operator".to_string(),
                    )),
                }
            }
            Expr::Binary { left, right, op } => {
                let left_val = self.interpret(*left)?;
                let right_val = self.interpret(*right)?;

                match (op.t, left_val, right_val) {
                    // subtraction
                    (TokenType::Minus, LoxValue::Number(a), LoxValue::Number(b)) => {
                        Ok(LoxValue::Number(a - b))
                    }
                    (TokenType::Minus, _, _) => Err(InterpreterError::from_token(
                        &op,
                        "Subraction can only be done on numbers".to_string(),
                    )),

                    // division
                    (TokenType::Slash, LoxValue::Number(a), LoxValue::Number(b)) => {
                        Ok(LoxValue::Number(a / b))
                    }
                    (TokenType::Slash, _, _) => Err(InterpreterError::from_token(
                        &op,
                        "Division can only be done on numbers".to_string(),
                    )),

                    // multiplication
                    (TokenType::Star, LoxValue::Number(a), LoxValue::Number(b)) => {
                        Ok(LoxValue::Number(a * b))
                    }
                    (TokenType::Star, _, _) => Err(InterpreterError::from_token(
                        &op,
                        "Multiplication can only be done on numbers".to_string(),
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
                        "The + operator can be used to add two numbers or two strings only"
                            .to_string(),
                    )),

                    // greater than
                    (TokenType::Greater, LoxValue::Number(a), LoxValue::Number(b)) => {
                        Ok(LoxValue::Boolean(a > b))
                    }
                    (TokenType::Greater, _, _) => Err(InterpreterError::from_token(
                        &op,
                        "Greater than comparison can only be done on numbers".to_string(),
                    )),

                    // greater than or equal
                    (TokenType::GreaterEqual, LoxValue::Number(a), LoxValue::Number(b)) => {
                        Ok(LoxValue::Boolean(a >= b))
                    }
                    (TokenType::GreaterEqual, _, _) => Err(InterpreterError::from_token(
                        &op,
                        "Greater than or equal comparison can only be done on numbers".to_string(),
                    )),

                    // less than
                    (TokenType::Less, LoxValue::Number(a), LoxValue::Number(b)) => {
                        Ok(LoxValue::Boolean(a < b))
                    }
                    (TokenType::Less, _, _) => Err(InterpreterError::from_token(
                        &op,
                        "Less than comparison can only be done on numbers".to_string(),
                    )),

                    // less than or equal
                    (TokenType::LessEqual, LoxValue::Number(a), LoxValue::Number(b)) => {
                        Ok(LoxValue::Boolean(a < b))
                    }
                    (TokenType::LessEqual, _, _) => Err(InterpreterError::from_token(
                        &op,
                        "Less than or equal comparison can only be done on numbers".to_string(),
                    )),

                    // not equal
                    (TokenType::BangEqual, a, b) => Ok(LoxValue::Boolean(a != b)),

                    // equal
                    (TokenType::EqualEqual, a, b) => Ok(LoxValue::Boolean(a == b)),

                    // fall through
                    _ => Err(InterpreterError::from_token(
                        &op,
                        "Unknown binary operator".to_string(),
                    )),
                }
            }
            Expr::Grouping { expression } => self.interpret(*expression),
        }
    }
}
