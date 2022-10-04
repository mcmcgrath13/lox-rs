use std::cell::RefCell;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::ast::{Expr, Stmt};
use crate::environment::Environment;
use crate::token::{Token, TokenType};
use crate::types::{Callable, LoxValue, NativeFunction, UserFunction};
use crate::Reportable;

fn is_truthy(val: &LoxValue) -> bool {
    match val {
        LoxValue::Boolean(b) => *b,
        LoxValue::Nil => false,
        _ => true,
    }
}

pub enum InterpreterError {
    Exception {
        line: usize,
        location: String,
        message: String,
    },
    Return {
        value: LoxValue,
    },
}

impl InterpreterError {
    pub fn new(line: usize, location: impl AsRef<str>, message: impl AsRef<str>) -> Self {
        Self::Exception {
            line,
            location: location.as_ref().to_string(),
            message: message.as_ref().to_string(),
        }
    }

    pub fn from_token(token: &Token, message: impl AsRef<str>) -> Self {
        Self::Exception {
            line: token.line,
            location: token.lexeme.to_string(),
            message: message.as_ref().to_string(),
        }
    }

    pub fn from_result(value: LoxValue) -> Self {
        Self::Return { value }
    }
}

impl Reportable for InterpreterError {
    fn report(&self) {
        match self {
            InterpreterError::Exception {
                line,
                location,
                message,
            } => {
                eprintln!(
                    "[line {} at {}] Error (Interpreter): {}",
                    line, location, message
                );
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
    globals: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::new(Environment::new(None)));

        // move this if there are ever more native functions
        globals.borrow_mut().define_name(
            "clock",
            LoxValue::Function(Rc::new(Box::new(NativeFunction::new(
                |_arguments: &[LoxValue]| -> Result<LoxValue, String> {
                    let start = SystemTime::now();
                    let since_the_epoch = start
                        .duration_since(UNIX_EPOCH)
                        .expect("Time went backwards");
                    Ok(LoxValue::Number(since_the_epoch.as_secs() as f64))
                },
                0,
            )))),
        );

        Self {
            globals: Rc::clone(&globals),
            environment: Rc::clone(&globals),
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
                self.execute_block(&statements, Rc::clone(&environment))?;
            }
            Stmt::Expression { expression } => {
                self.evaluate(expression, Rc::clone(&environment))?;
            }
            Stmt::Function {
                name,
                parameters,
                body,
            } => {
                let function = LoxValue::Function(Rc::new(Box::new(UserFunction::new(
                    name.clone(),
                    parameters,
                    body,
                    Rc::clone(&environment),
                ))));
                environment.borrow_mut().define(&name, function);
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
            Stmt::Return { value, .. } => {
                let result = match value {
                    None => LoxValue::Nil,
                    Some(expr) => self.evaluate(expr, Rc::clone(&environment))?,
                };
                // this is hacking into error bubbling up for the return code path
                // not a true exception
                return Err(InterpreterError::from_result(result));
            }
            Stmt::Var { name, initializer } => {
                let mut value = LoxValue::Nil;
                if let Some(expr) = initializer {
                    value = self.evaluate(expr, Rc::clone(&environment))?;
                }

                environment.borrow_mut().define(&name, value);
            }
            Stmt::While { condition, body } => {
                while is_truthy(&self.evaluate(condition.clone(), Rc::clone(&environment))?) {
                    self.execute(*body.clone(), Rc::clone(&environment))?;
                }
            }
        };

        Ok(())
    }

    pub fn execute_block(
        &self,
        statements: &Vec<Stmt>,
        environment: Rc<RefCell<Environment>>,
    ) -> Result<(), InterpreterError> {
        let block_environment = Rc::new(RefCell::new(Environment::new(Some(Rc::clone(
            &environment,
        )))));
        for statement in statements {
            self.execute(statement.clone(), Rc::clone(&block_environment))?;
        }
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
            Expr::Call {
                callee,
                paren,
                arguments,
            } => {
                let callable = self.evaluate(*callee, Rc::clone(&environment))?;

                let mut argument_vals = Vec::new();
                for argument in arguments {
                    argument_vals.push(self.evaluate(argument, Rc::clone(&environment))?);
                }

                callable.call(self, &argument_vals, paren.line)
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
