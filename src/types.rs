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
    Function(UserFunction),
    Builtin(NativeFunction),
    Class(Class),
    Instance(Instance),
}

impl LoxValue {
    pub fn bind(&self, instance: Instance) -> Result<Self, InterpreterError> {
        match self {
            LoxValue::Function(f) => Ok(LoxValue::Function(f.bind(instance))),
            _ => Err(InterpreterError::new(
                0,
                "somewhere",
                "can only bind an instance to a function",
            )),
        }
    }
}

impl Callable for LoxValue {
    fn arity(&self) -> usize {
        match self {
            LoxValue::Class(c) => c.arity(),
            LoxValue::Function(f) => f.arity(),
            LoxValue::Builtin(f) => f.arity(),
            _ => 0,
        }
    }

    fn location(&self) -> String {
        match self {
            LoxValue::Class(c) => c.location(),
            LoxValue::Function(f) => f.location(),
            LoxValue::Builtin(f) => f.location(),
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
            LoxValue::Class(callable) => callable.call(interpreter, arguments, line, locals),
            LoxValue::Function(callable) => callable.call(interpreter, arguments, line, locals),
            LoxValue::Builtin(callable) => callable.call(interpreter, arguments, line, locals),
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
            LoxValue::Builtin(fun) => write!(f, "{}", fun.location()),
            LoxValue::Class(c) => write!(f, "<class {}>", c.location()),
            LoxValue::Instance(i) => write!(f, "<instance of {}>", i.name),
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

    pub fn bind(&self, instance: Instance) -> Self {
        let environment = Rc::new(RefCell::new(Environment::new(Some(Rc::clone(
            &self.closure,
        )))));
        environment
            .borrow_mut()
            .define_name("this", LoxValue::Instance(instance));
        UserFunction::new(
            self.name.clone(),
            self.parameters.clone(),
            self.body.clone(),
            environment,
        )
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

// ========== CLASS ===========

#[derive(Clone, Debug)]
pub struct Class {
    name: Token,
    methods: Rc<RefCell<HashMap<String, LoxValue>>>,
}

impl Class {
    pub fn new(name: Token, methods: HashMap<String, LoxValue>) -> Self {
        Self {
            name,
            methods: Rc::new(RefCell::new(methods)),
        }
    }
}

impl Callable for Class {
    fn arity(&self) -> usize {
        0
    }

    fn location(&self) -> String {
        format!("<fn {}>", self.name.lexeme)
    }

    fn call(
        &self,
        _interpreter: &Interpreter,
        arguments: &[LoxValue],
        line: usize,
        _locals: &HashMap<Token, usize>,
    ) -> Result<LoxValue, InterpreterError> {
        self.check_arity(arguments, line)?;

        Ok(LoxValue::Instance(Instance::new(
            self.name.lexeme.to_string(),
            Rc::clone(&self.methods),
        )))
    }
}

// ========== INSTANCE ===========
#[derive(Clone, Debug)]
pub struct Instance {
    name: String,
    fields: Rc<RefCell<HashMap<String, LoxValue>>>,
    methods: Rc<RefCell<HashMap<String, LoxValue>>>,
}

impl Instance {
    pub fn new(name: String, methods: Rc<RefCell<HashMap<String, LoxValue>>>) -> Self {
        Self {
            name,
            fields: Rc::new(RefCell::new(HashMap::new())),
            methods,
        }
    }

    pub fn get(&self, name: &Token) -> Result<LoxValue, InterpreterError> {
        if let Some(v) = self.fields.borrow().get(&name.lexeme) {
            return Ok(v.clone());
        }

        if let Some(m) = self.find_method(name) {
            return m.bind(self.clone());
        };

        Err(InterpreterError::from_token(name, "undefined property"))
    }

    pub fn set(&mut self, name: &Token, value: LoxValue) {
        self.fields
            .borrow_mut()
            .insert(name.lexeme.to_string(), value);
    }

    pub fn find_method(&self, name: &Token) -> Option<LoxValue> {
        self.methods.borrow().get(&name.lexeme).cloned()
    }
}
