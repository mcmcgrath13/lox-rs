use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::ast::Stmt;
use crate::environment::Environment;
use crate::interpreter::{Interpreter, InterpreterError};
use crate::token::{Token, TokenType};

pub trait Callable: fmt::Debug {
    fn arity(&self) -> usize;
    fn location(&self) -> String;
    fn check_arity(&self, arguments: &[LoxValue], line: usize) -> Result<(), InterpreterError> {
        if self.arity() == arguments.len() {
            Ok(())
        } else {
            Err(InterpreterError::new(
                line,
                self.location(),
                "arity mismatch",
            ))
        }
    }
    fn call(
        &self,
        interpreter: &Interpreter,
        arguments: &[LoxValue],
        line: usize,
        locals: &HashMap<Token, usize>,
    ) -> Result<LoxValue, InterpreterError>;
}

// ========== LOX VALUE ===========

#[derive(Debug, Clone)]
pub enum LoxValue {
    Boolean(bool),
    Nil,
    Number(f64),
    String(String),
    Function(Rc<Box<dyn Callable>>),
}

impl Callable for LoxValue {
    fn arity(&self) -> usize {
        match self {
            LoxValue::Function(f) => f.arity(),
            _ => 0,
        }
    }

    fn location(&self) -> String {
        match self {
            LoxValue::Function(f) => f.location(),
            _ => "".to_string(),
        }
    }

    fn call(
        &self,
        interpreter: &Interpreter,
        arguments: &[LoxValue],
        line: usize,
        locals: &HashMap<Token, usize>,
    ) -> Result<LoxValue, InterpreterError> {
        match self {
            LoxValue::Function(callable) => callable.call(interpreter, arguments, line, locals),
            _ => Err(InterpreterError::new(
                line,
                self.location(),
                "can only call functions and classes",
            )),
        }
    }
}

impl PartialEq for LoxValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LoxValue::Nil, LoxValue::Nil) => false,
            (LoxValue::Boolean(a), LoxValue::Boolean(b)) => a == b,
            (LoxValue::Number(a), LoxValue::Number(b)) => a == b,
            (LoxValue::String(a), LoxValue::String(b)) => a == b,
            (_, _) => false,
        }
    }
}
impl Eq for LoxValue {}

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
            LoxValue::Function(fun) => write!(f, "<fun {}>", fun.location()),
        }
    }
}

// ========== NATIVE FUNCTION ===========

type FnPtr = fn(&[LoxValue]) -> Result<LoxValue, String>;

#[derive(Clone)]
pub struct NativeFunction {
    arity: usize,
    body: FnPtr,
}

impl NativeFunction {
    pub fn new(body: FnPtr, arity: usize) -> Self {
        Self { arity, body }
    }
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.location())
    }
}

impl Callable for NativeFunction {
    fn arity(&self) -> usize {
        self.arity
    }

    fn location(&self) -> String {
        "<native fn>".to_string()
    }

    fn call(
        &self,
        _interpreter: &Interpreter,
        arguments: &[LoxValue],
        line: usize,
        _locals: &HashMap<Token, usize>,
    ) -> Result<LoxValue, InterpreterError> {
        self.check_arity(arguments, line)?;
        match (self.body)(arguments) {
            Ok(v) => Ok(v),
            Err(s) => Err(InterpreterError::new(line, self.location(), s)),
        }
    }
}

// ========== USER FUNCTION ===========

#[derive(Clone, Debug)]
pub struct UserFunction {
    name: Token,
    parameters: Vec<Token>,
    body: Vec<Stmt>,
    closure: Rc<RefCell<Environment>>,
}

impl UserFunction {
    pub fn new(
        name: Token,
        parameters: Vec<Token>,
        body: Vec<Stmt>,
        closure: Rc<RefCell<Environment>>,
    ) -> Self {
        Self {
            name,
            parameters,
            body,
            closure: Rc::clone(&closure),
        }
    }
}

impl Callable for UserFunction {
    fn arity(&self) -> usize {
        self.parameters.len()
    }

    fn location(&self) -> String {
        format!("<fn {}>", self.name.lexeme)
    }

    fn call(
        &self,
        interpreter: &Interpreter,
        arguments: &[LoxValue],
        line: usize,
        locals: &HashMap<Token, usize>,
    ) -> Result<LoxValue, InterpreterError> {
        self.check_arity(arguments, line)?;

        let environment = Rc::new(RefCell::new(Environment::new(Some(Rc::clone(
            &self.closure,
        )))));

        // arity has already been checked, so unwrapping is safe
        for i in 0..self.arity() {
            environment.borrow_mut().define(
                self.parameters.get(i).unwrap(),
                arguments.get(i).unwrap().clone(),
            );
        }

        match interpreter.execute_block(&self.body, Rc::clone(&environment), locals) {
            Ok(()) => Ok(LoxValue::Nil),
            Err(e) => match e {
                InterpreterError::Return { value } => Ok(value),
                _ => Err(e),
            },
        }
    }
}
