use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::ast::{Expr, Stmt};
use crate::environment::Environment;
use crate::token::{Token, TokenType};
use crate::types::{Callable, Class, FindMethod, LoxValue, NativeFunction, UserFunction};
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
    fn report(&self) -> String {
        match self {
            InterpreterError::Exception {
                line,
                location,
                message,
            } => {
                format!(
                    "[line {} at {}] Error (Interpreter): {}",
                    line, location, message
                )
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
    globals: Rc<RefCell<Environment>>,
    outputs: Vec<String>,
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::new(Environment::new(None)));

        // move this if there are ever more native functions
        globals.borrow_mut().define(
            "clock",
            LoxValue::Builtin(NativeFunction::new(
                |_arguments: &[Rc<LoxValue>]| -> Result<LoxValue, String> {
                    let start = SystemTime::now();
                    let since_the_epoch = start
                        .duration_since(UNIX_EPOCH)
                        .expect("Time went backwards");
                    Ok(LoxValue::Number(since_the_epoch.as_secs() as f64))
                },
                0,
            )),
        );

        Self {
            globals: Rc::clone(&globals),
            environment: Rc::clone(&globals),
            outputs: Vec::new(),
        }
    }

    pub fn interpret(
        &mut self,
        stmts: Vec<Stmt>,
        locals: &HashMap<Token, usize>,
    ) -> Result<String, InterpreterError> {
        self.outputs.clear();
        for stmt in &stmts {
            self.execute(stmt, Rc::clone(&self.environment), locals)?;
        }

        Ok(self.outputs.join("\n"))
    }

    fn execute(
        &mut self,
        stmt: &Stmt,
        environment: Rc<RefCell<Environment>>,
        locals: &HashMap<Token, usize>,
    ) -> Result<(), InterpreterError> {
        match stmt {
            Stmt::Block { statements } => {
                self.execute_block(statements, Rc::clone(&environment), locals)?;
            }
            Stmt::Class {
                name,
                super_class,
                methods,
            } => {
                let (super_class_class, class_environment) =
                    if let Some(super_class_expr) = super_class {
                        let super_class_value =
                            self.evaluate(super_class_expr, Rc::clone(&environment), locals)?;
                        let super_class_class = match &super_class_value.as_ref() {
                            LoxValue::Class(c) => Some(Rc::clone(c)),
                            _ => {
                                return Err(InterpreterError::from_token(
                                    name,
                                    "super class must be a class",
                                ))
                            }
                        };

                        environment.borrow_mut().define(name, LoxValue::Nil);

                        let env = Rc::new(RefCell::new(Environment::new(Some(Rc::clone(
                            &environment,
                        )))));
                        env.borrow_mut()
                            .define("super", super_class_value.as_ref().clone()); // TODO: pass
                                                                                  // down RC

                        (super_class_class, env)
                    } else {
                        environment.borrow_mut().define(name, LoxValue::Nil);
                        (None, Rc::clone(&environment))
                    };

                let mut method_map = HashMap::new();
                for method in methods {
                    match method {
                        Stmt::Function {
                            body,
                            parameters,
                            name: method_name,
                        } => {
                            let function = LoxValue::Function(UserFunction::new(
                                method_name.clone(),
                                parameters.clone(),
                                body.clone(),
                                Rc::clone(&class_environment),
                                method_name.lexeme == "init",
                            ));
                            method_map.insert(method_name.lexeme.to_string(), function);
                        }
                        _ => {
                            return Err(InterpreterError::from_token(
                                name,
                                "only methods allowed in classes",
                            ));
                        }
                    }
                }

                let class = LoxValue::Class(Rc::new(RefCell::new(Class::new(
                    name.clone(),
                    super_class_class,
                    method_map,
                ))));

                environment.borrow_mut().assign(name, class);
            }
            Stmt::Expression { expression } => {
                self.evaluate(expression, Rc::clone(&environment), locals)?;
            }
            Stmt::Function {
                name,
                parameters,
                body,
            } => {
                let function = LoxValue::Function(UserFunction::new(
                    name.clone(),
                    parameters.clone(),
                    body.clone(),
                    Rc::clone(&environment),
                    false,
                ));
                environment.borrow_mut().define(name, function);
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if is_truthy(
                    &self
                        .evaluate(condition, Rc::clone(&environment), locals)?
                        .as_ref()
                        .clone(),
                ) {
                    self.execute(then_branch, Rc::clone(&environment), locals)?;
                } else if let Some(e) = else_branch {
                    self.execute(e, Rc::clone(&environment), locals)?;
                }
            }
            Stmt::Print { expression } => {
                let value = self.evaluate(expression, Rc::clone(&environment), locals)?;
                self.outputs.push(format!("{}", value));
            }
            Stmt::Return { value, .. } => {
                let result = match value {
                    None => LoxValue::Nil,
                    Some(expr) => self
                        .evaluate(expr, Rc::clone(&environment), locals)?
                        .as_ref()
                        .clone(),
                };
                // this is hacking into error bubbling up for the return code path
                // not a true exception
                return Err(InterpreterError::from_result(result));
            }
            Stmt::Var { name, initializer } => {
                let mut value = LoxValue::Nil;
                if let Some(expr) = initializer {
                    value = self
                        .evaluate(expr, Rc::clone(&environment), locals)?
                        .as_ref()
                        .clone();
                }

                environment.borrow_mut().define(name, value);
            }
            Stmt::While { condition, body } => {
                while is_truthy(
                    &self
                        .evaluate(condition, Rc::clone(&environment), locals)?
                        .as_ref()
                        .clone(),
                ) {
                    self.execute(body, Rc::clone(&environment), locals)?;
                }
            }
        };

        Ok(())
    }

    pub fn execute_block(
        &mut self,
        statements: &Vec<Stmt>,
        environment: Rc<RefCell<Environment>>,
        locals: &HashMap<Token, usize>,
    ) -> Result<(), InterpreterError> {
        let block_environment = Rc::new(RefCell::new(Environment::new(Some(Rc::clone(
            &environment,
        )))));
        for statement in statements {
            self.execute(statement, Rc::clone(&block_environment), locals)?;
        }
        Ok(())
    }

    fn evaluate(
        &mut self,
        expr: &Expr,
        environment: Rc<RefCell<Environment>>,
        locals: &HashMap<Token, usize>,
    ) -> Result<Rc<LoxValue>, InterpreterError> {
        match expr {
            Expr::Assign { name, value } => {
                let v = self.evaluate(value, Rc::clone(&environment), locals)?;
                match locals.get(name) {
                    Some(distance) => {
                        if environment
                            .borrow_mut()
                            .assign_at(*distance, name, v.as_ref().clone())
                            .is_none()
                        {
                            return Err(InterpreterError::from_token(name, "undefined variable"));
                        }
                    }
                    None => {
                        if self
                            .globals
                            .borrow_mut()
                            .assign(name, v.as_ref().clone())
                            .is_none()
                        {
                            return Err(InterpreterError::from_token(name, "undefined variable"));
                        }
                    }
                }

                Ok(v)
            }
            Expr::Binary { left, right, op } => {
                let left_val = self.evaluate(left, Rc::clone(&environment), locals)?;
                let right_val = self.evaluate(right, Rc::clone(&environment), locals)?;

                match (&op.t, left_val.as_ref(), right_val.as_ref()) {
                    // subtraction
                    (TokenType::Minus, LoxValue::Number(a), LoxValue::Number(b)) => {
                        Ok(Rc::new(LoxValue::Number(a - b)))
                    }
                    (TokenType::Minus, _, _) => Err(InterpreterError::from_token(
                        op,
                        "Subraction can only be done on numbers",
                    )),

                    // division
                    (TokenType::Slash, LoxValue::Number(a), LoxValue::Number(b)) => {
                        Ok(Rc::new(LoxValue::Number(a / b)))
                    }
                    (TokenType::Slash, _, _) => Err(InterpreterError::from_token(
                        op,
                        "Division can only be done on numbers",
                    )),

                    // multiplication
                    (TokenType::Star, LoxValue::Number(a), LoxValue::Number(b)) => {
                        Ok(Rc::new(LoxValue::Number(a * b)))
                    }
                    (TokenType::Star, _, _) => Err(InterpreterError::from_token(
                        op,
                        "Multiplication can only be done on numbers",
                    )),

                    // addition
                    (TokenType::Plus, LoxValue::Number(a), LoxValue::Number(b)) => {
                        Ok(Rc::new(LoxValue::Number(a + b)))
                    }
                    (TokenType::Plus, LoxValue::String(a), LoxValue::String(b)) => {
                        Ok(Rc::new(LoxValue::String(format!("{}{}", a, b))))
                    }
                    (TokenType::Plus, _, _) => Err(InterpreterError::from_token(
                        op,
                        "The + operator can be used to add two numbers or two strings only",
                    )),

                    // greater than
                    (TokenType::Greater, LoxValue::Number(a), LoxValue::Number(b)) => {
                        Ok(Rc::new(LoxValue::Boolean(a > b)))
                    }
                    (TokenType::Greater, _, _) => Err(InterpreterError::from_token(
                        op,
                        "Greater than comparison can only be done on numbers",
                    )),

                    // greater than or equal
                    (TokenType::GreaterEqual, LoxValue::Number(a), LoxValue::Number(b)) => {
                        Ok(Rc::new(LoxValue::Boolean(a >= b)))
                    }
                    (TokenType::GreaterEqual, _, _) => Err(InterpreterError::from_token(
                        op,
                        "Greater than or equal comparison can only be done on numbers",
                    )),

                    // less than
                    (TokenType::Less, LoxValue::Number(a), LoxValue::Number(b)) => {
                        Ok(Rc::new(LoxValue::Boolean(a < b)))
                    }
                    (TokenType::Less, _, _) => Err(InterpreterError::from_token(
                        op,
                        "Less than comparison can only be done on numbers",
                    )),

                    // less than or equal
                    (TokenType::LessEqual, LoxValue::Number(a), LoxValue::Number(b)) => {
                        Ok(Rc::new(LoxValue::Boolean(a <= b)))
                    }
                    (TokenType::LessEqual, _, _) => Err(InterpreterError::from_token(
                        op,
                        "Less than or equal comparison can only be done on numbers",
                    )),

                    // not equal
                    (TokenType::BangEqual, a, b) => Ok(Rc::new(LoxValue::Boolean(a != b))),

                    // equal
                    (TokenType::EqualEqual, a, b) => Ok(Rc::new(LoxValue::Boolean(a == b))),

                    // fall through
                    _ => Err(InterpreterError::from_token(op, "Unknown binary operator")),
                }
            }
            Expr::Call {
                callee,
                paren,
                arguments,
            } => {
                let callable = self.evaluate(callee, Rc::clone(&environment), locals)?;

                let mut argument_vals = Vec::new();
                for argument in arguments {
                    argument_vals.push(self.evaluate(argument, Rc::clone(&environment), locals)?);
                }

                callable.call(self, &argument_vals, paren.line, locals)
            }
            Expr::Get { object, name } => {
                let object_val = self.evaluate(object, Rc::clone(&environment), locals)?;
                match object_val.as_ref() {
                    LoxValue::Instance(i) => Ok(i.get(name)?),
                    _ => Err(InterpreterError::from_token(
                        name,
                        "only instances have properties",
                    )),
                }
            }
            Expr::Grouping { expression } => {
                self.evaluate(expression, Rc::clone(&environment), locals)
            }
            Expr::Literal { value } => {
                let val: Result<LoxValue, InterpreterError> = value.try_into();
                val.map(Rc::new)
            }
            Expr::Logical { left, op, right } => {
                let left_val = self.evaluate(left, Rc::clone(&environment), locals)?;
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
                    _ => return Err(InterpreterError::from_token(op, "Unknown logical operator")),
                }

                Ok(self.evaluate(right, Rc::clone(&environment), locals)?)
            }
            Expr::Set {
                object,
                name,
                value,
            } => match self
                .evaluate(object, Rc::clone(&environment), locals)?
                .as_ref()
            {
                LoxValue::Instance(i) => {
                    let value_val = self.evaluate(value, Rc::clone(&environment), locals)?;
                    let mut i = i.clone();
                    i.set(name, value_val.as_ref().clone());
                    Ok(value_val)
                }
                _ => Err(InterpreterError::from_token(
                    name,
                    "only instances have fields",
                )),
            },
            Expr::Super { keyword, method } => {
                let super_class = self.lookup_variable(keyword, Rc::clone(&environment), locals)?;
                let distance = locals.get(keyword).expect("super always in a nested scope");
                let this = environment
                    .borrow()
                    .get_at(distance - 1, "this")
                    .expect("this always one off from super");
                let instance = match this.as_ref() {
                    LoxValue::Instance(i) => i,
                    _ => panic!("this doesn't refer to an instance"),
                };
                let super_class_class = match super_class.as_ref() {
                    LoxValue::Class(c) => c,
                    _ => panic!("super referred to non-class"),
                };
                let res = match super_class_class.borrow().find_method(method) {
                    Some(m) => m.bind(instance.clone()),
                    None => Err(InterpreterError::from_token(
                        method,
                        "method not found in super class",
                    )),
                };
                res
            }
            Expr::This { keyword } => {
                self.lookup_variable(keyword, Rc::clone(&environment), locals)
            }
            Expr::Unary { op, right } => {
                let right_val = self.evaluate(right, Rc::clone(&environment), locals)?;
                match (&op.t, right_val.as_ref()) {
                    (TokenType::Minus, LoxValue::Number(n)) => {
                        Ok(Rc::new(LoxValue::Number(-1.0 * n)))
                    }
                    (TokenType::Minus, _) => Err(InterpreterError::from_token(
                        op,
                        "non-number with unary minus",
                    )),
                    (TokenType::Bang, v) => Ok(Rc::new(LoxValue::Boolean(is_truthy(v)))),
                    _ => Err(InterpreterError::from_token(op, "Unknown unary operator")),
                }
            }
            Expr::Variable { name } => self.lookup_variable(name, Rc::clone(&environment), locals),
        }
    }

    fn lookup_variable(
        &self,
        name: &Token,
        environment: Rc<RefCell<Environment>>,
        locals: &HashMap<Token, usize>,
    ) -> Result<Rc<LoxValue>, InterpreterError> {
        match locals.get(name) {
            Some(distance) => match environment.borrow().get_at(*distance, name) {
                Some(v) => Ok(v),
                None => panic!("resolver doesn't match interpreter!!!"),
            },
            None => match self.globals.borrow().get(name) {
                Some(v) => Ok(v),
                None => Err(InterpreterError::from_token(name, "undefined variable")),
            },
        }
    }
}
