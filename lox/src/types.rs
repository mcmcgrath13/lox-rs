use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::ast::Stmt;
use crate::environment::Environment;
use crate::interpreter::{Interpreter, InterpreterError};
use crate::token::{Token, TokenType};
use crate::PrettyPrinting;

pub trait Callable: fmt::Debug {
    fn arity(&self) -> usize;
    fn location(&self) -> String;
    fn check_arity(&self, arguments: &[Rc<LoxValue>], line: usize) -> Result<(), InterpreterError> {
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
        interpreter: &mut Interpreter,
        arguments: &[Rc<LoxValue>],
        line: usize,
        locals: &HashMap<Token, usize>,
    ) -> Result<Rc<LoxValue>, InterpreterError>;
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
    Class(Rc<RefCell<Class>>),
    Instance(Instance),
}

impl LoxValue {
    pub fn bind(&self, instance: Instance) -> Result<Rc<Self>, InterpreterError> {
        match self {
            LoxValue::Function(f) => Ok(Rc::new(LoxValue::Function(f.bind(instance)))),
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
            LoxValue::Class(c) => c.borrow().arity(),
            LoxValue::Function(f) => f.arity(),
            LoxValue::Builtin(f) => f.arity(),
            _ => 0,
        }
    }

    fn location(&self) -> String {
        match self {
            LoxValue::Class(c) => c.borrow().location(),
            LoxValue::Function(f) => f.location(),
            LoxValue::Builtin(f) => f.location(),
            _ => "".to_string(),
        }
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: &[Rc<LoxValue>],
        line: usize,
        locals: &HashMap<Token, usize>,
    ) -> Result<Rc<LoxValue>, InterpreterError> {
        match self {
            LoxValue::Class(callable) => {
                callable.borrow().call(interpreter, arguments, line, locals)
            }
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
            _ => Err(InterpreterError::from_token(
                value,
                "Unexpected literal value",
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
            LoxValue::Function(fun) => write!(f, "<fun {}>", fun.location()),
            LoxValue::Builtin(fun) => write!(f, "{}", fun.location()),
            LoxValue::Class(c) => write!(f, "<class {}>", c.borrow().location()),
            LoxValue::Instance(i) => write!(f, "<instance of {}>", i.name),
        }
    }
}

// ========== NATIVE FUNCTION ===========

type FnPtr = fn(&[Rc<LoxValue>]) -> Result<LoxValue, String>;

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
        _interpreter: &mut Interpreter,
        arguments: &[Rc<LoxValue>],
        line: usize,
        _locals: &HashMap<Token, usize>,
    ) -> Result<Rc<LoxValue>, InterpreterError> {
        self.check_arity(arguments, line)?;
        match (self.body)(arguments) {
            Ok(v) => Ok(v.into()),
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
    is_initializer: bool,
}

impl UserFunction {
    pub fn new(
        name: Token,
        parameters: Vec<Token>,
        body: Vec<Stmt>,
        closure: Rc<RefCell<Environment>>,
        is_initializer: bool,
    ) -> Self {
        Self {
            name,
            parameters,
            body,
            closure: Rc::clone(&closure),
            is_initializer,
        }
    }

    pub fn bind(&self, instance: Instance) -> Self {
        let environment = Rc::new(RefCell::new(Environment::new(Some(Rc::clone(
            &self.closure,
        )))));
        environment
            .borrow_mut()
            .define("this", LoxValue::Instance(instance));
        UserFunction::new(
            self.name.clone(),
            self.parameters.clone(),
            self.body.clone(),
            environment,
            self.is_initializer,
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
        interpreter: &mut Interpreter,
        arguments: &[Rc<LoxValue>],
        line: usize,
        locals: &HashMap<Token, usize>,
    ) -> Result<Rc<LoxValue>, InterpreterError> {
        self.check_arity(arguments, line)?;

        let environment = Rc::new(RefCell::new(Environment::new(Some(Rc::clone(
            &self.closure,
        )))));

        // arity has already been checked, so unwrapping is safe
        for i in 0..self.arity() {
            environment.borrow_mut().define(
                self.parameters.get(i).unwrap(),
                arguments.get(i).unwrap().as_ref().clone(),
            );
        }

        let mut return_value = Rc::new(LoxValue::Nil);
        if let Err(e) = interpreter.execute_block(&self.body, Rc::clone(&environment), locals) {
            match e {
                InterpreterError::Return { value } => {
                    return_value = Rc::new(value);
                }
                _ => return Err(e),
            }
        }

        if self.is_initializer {
            match self.closure.borrow().get_at(0, "this") {
                Some(v) => {
                    return_value = v;
                }
                None => {
                    return Err(InterpreterError::from_token(
                        &self.name,
                        "'this' not found in closure of initializing method",
                    ))
                }
            }
        }

        Ok(return_value)
    }
}

// ========== FIND METHOD ==========

pub trait FindMethod {
    fn methods(&self) -> Rc<RefCell<HashMap<String, LoxValue>>>;
    fn super_class(&self) -> Option<Rc<RefCell<Class>>>;
    fn find_method(&self, name: impl AsRef<str>) -> Option<LoxValue> {
        match self.methods().borrow().get(name.as_ref()) {
            Some(v) => Some(v.clone()),
            None => match self.super_class() {
                Some(c) => c.borrow().find_method(name),
                None => None,
            },
        }
    }
}

// ========== CLASS ===========

#[derive(Clone, Debug)]
pub struct Class {
    name: Token,
    super_class: Option<Rc<RefCell<Class>>>,
    methods: Rc<RefCell<HashMap<String, LoxValue>>>,
}

impl Class {
    pub fn new(
        name: Token,
        super_class: Option<Rc<RefCell<Class>>>,
        methods: HashMap<String, LoxValue>,
    ) -> Self {
        Self {
            name,
            super_class,
            methods: Rc::new(RefCell::new(methods)),
        }
    }
}

impl FindMethod for Class {
    fn methods(&self) -> Rc<RefCell<HashMap<String, LoxValue>>> {
        Rc::clone(&self.methods)
    }
    fn super_class(&self) -> Option<Rc<RefCell<Class>>> {
        self.super_class.as_ref().map(Rc::clone)
    }
}

impl Callable for Class {
    fn arity(&self) -> usize {
        match self.find_method("init") {
            Some(v) => v.arity(),
            None => 0,
        }
    }

    fn location(&self) -> String {
        self.name.print()
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: &[Rc<LoxValue>],
        line: usize,
        locals: &HashMap<Token, usize>,
    ) -> Result<Rc<LoxValue>, InterpreterError> {
        self.check_arity(arguments, line)?;

        let instance = Instance::new(
            &self.name,
            self.super_class.as_ref().map(Rc::clone),
            Rc::clone(&self.methods),
        );

        if let Some(initializer) = self.find_method("init") {
            initializer
                .bind(instance.clone())?
                .call(interpreter, arguments, line, locals)?;
        }

        Ok(Rc::new(LoxValue::Instance(instance)))
    }
}

// ========== INSTANCE ===========
#[derive(Clone, Debug)]
pub struct Instance {
    name: String,
    super_class: Option<Rc<RefCell<Class>>>,
    fields: Rc<RefCell<HashMap<String, LoxValue>>>,
    methods: Rc<RefCell<HashMap<String, LoxValue>>>,
}

impl Instance {
    pub fn new(
        name: impl AsRef<str>,
        super_class: Option<Rc<RefCell<Class>>>,
        methods: Rc<RefCell<HashMap<String, LoxValue>>>,
    ) -> Self {
        Self {
            name: name.as_ref().to_string(),
            super_class,
            fields: Rc::new(RefCell::new(HashMap::new())),
            methods,
        }
    }

    pub fn get(&self, name: &Token) -> Result<Rc<LoxValue>, InterpreterError> {
        if let Some(v) = self.fields.borrow().get(name.as_ref()) {
            return Ok(Rc::new(v.clone()));
        }

        if let Some(m) = self.find_method(name) {
            return m.bind(self.clone());
        };

        Err(InterpreterError::from_token(name, "undefined property"))
    }

    pub fn set(&mut self, name: impl AsRef<str>, value: LoxValue) {
        self.fields
            .borrow_mut()
            .insert(name.as_ref().to_string(), value);
    }
}

impl FindMethod for Instance {
    fn methods(&self) -> Rc<RefCell<HashMap<String, LoxValue>>> {
        Rc::clone(&self.methods)
    }
    fn super_class(&self) -> Option<Rc<RefCell<Class>>> {
        self.super_class.as_ref().map(Rc::clone)
    }
}
