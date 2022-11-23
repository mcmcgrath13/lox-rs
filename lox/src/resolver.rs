use std::collections::HashMap;

use crate::ast::{Expr, Stmt};
use crate::token::Token;
use crate::Reportable;

pub struct ResolveError {
    line: usize,
    location: String,
    message: String,
}

impl ResolveError {
    pub fn from_token(token: &Token, message: impl AsRef<str>) -> Self {
        Self {
            line: token.line,
            location: token.lexeme.to_string(),
            message: message.as_ref().to_string(),
        }
    }
}

impl Reportable for ResolveError {
    fn report(&self) -> String {
        format!(
            "[line {} at {}] Error (Parser): {}",
            self.line, self.location, self.message
        )
    }
}

#[derive(Clone, Copy)]
enum FunctionType {
    None,
    Function,
    Initializer,
    Method,
}

#[derive(Clone, Copy, PartialEq)]
enum ClassType {
    None,
    Class,
    SubClass,
}

pub struct Resolver {
    stack: Vec<HashMap<String, bool>>,
    locals: HashMap<Token, usize>,
    current_function: FunctionType,
    current_class: ClassType,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            locals: HashMap::new(),
            current_function: FunctionType::None,
            current_class: ClassType::None,
        }
    }

    pub fn resolve(
        &mut self,
        statements: &Vec<Stmt>,
    ) -> (&HashMap<Token, usize>, Vec<ResolveError>) {
        let mut errs = Vec::new();
        for statement in statements {
            if let Err(e) = self.resolve_statement(statement) {
                errs.push(e);
            }
        }

        (&self.locals, errs)
    }

    fn resolve_statement(&mut self, statement: &Stmt) -> Result<(), ResolveError> {
        match statement {
            Stmt::Block { statements } => {
                self.begin_scope();
                for statement in statements {
                    self.resolve_statement(statement)?;
                }
                self.end_scope();
            }
            Stmt::Class {
                name,
                super_class,
                methods,
            } => {
                let enclosing_class = self.current_class;
                self.current_class = ClassType::Class;

                self.declare(name)?;
                self.define(name);

                if let Some(Expr::Variable { name: token }) = super_class {
                    if token.lexeme != name.lexeme {
                        self.current_class = ClassType::SubClass;
                        self.resolve_local(token);
                        self.begin_scope();
                        self.define("super");
                    } else {
                        return Err(ResolveError::from_token(
                            token,
                            "a class can't inherit from itself",
                        ));
                    }
                }

                self.begin_scope();
                if let Some(scope) = self.stack.last_mut() {
                    scope.insert("this".to_string(), true);
                }

                for method in methods {
                    match method {
                        Stmt::Function {
                            parameters,
                            body,
                            name,
                        } => {
                            let function_type = if name.lexeme == "init" {
                                FunctionType::Initializer
                            } else {
                                FunctionType::Method
                            };
                            self.resolve_function(parameters, body, function_type)?;
                        }
                        _ => {
                            return Err(ResolveError::from_token(
                                name,
                                "only methods are allowed in classes",
                            ))
                        }
                    }
                }

                self.end_scope();

                if super_class.is_some() {
                    self.end_scope();
                }

                self.current_class = enclosing_class;
            }
            Stmt::Expression { expression } => {
                self.resolve_expression(expression)?;
            }
            Stmt::Function {
                name,
                parameters,
                body,
            } => {
                self.declare(name)?;
                self.define(name);
                self.resolve_function(parameters, body, FunctionType::Function)?;
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expression(condition)?;
                self.resolve_statement(then_branch)?;
                if let Some(e) = else_branch {
                    self.resolve_statement(e)?;
                }
            }
            Stmt::Print { expression } => {
                self.resolve_expression(expression)?;
            }
            Stmt::Return { value, keyword } => match (self.current_function, value) {
                (FunctionType::None, _) => {
                    return Err(ResolveError::from_token(
                        keyword,
                        "can't return from top level code",
                    ))
                }
                (FunctionType::Initializer, Some(_)) => {
                    return Err(ResolveError::from_token(
                        keyword,
                        "can't return a value from a class initializer",
                    ))
                }
                (_, Some(expression)) => {
                    self.resolve_expression(expression)?;
                }
                _ => {}
            },
            Stmt::Var { name, initializer } => {
                self.declare(name)?;
                if let Some(v) = initializer {
                    self.resolve_expression(v)?;
                }
                self.define(name);
            }
            Stmt::While { condition, body } => {
                self.resolve_expression(condition)?;
                self.resolve_statement(body)?;
            }
        };

        Ok(())
    }

    fn resolve_expression(&mut self, expression: &Expr) -> Result<(), ResolveError> {
        match expression {
            Expr::Assign { name, value } => {
                self.resolve_expression(value)?;
                self.resolve_local(name);
            }
            Expr::Binary { left, right, .. } => {
                self.resolve_expression(left)?;
                self.resolve_expression(right)?;
            }
            Expr::Call {
                callee, arguments, ..
            } => {
                self.resolve_expression(callee)?;

                for argument in arguments {
                    self.resolve_expression(argument)?;
                }
            }
            Expr::Get { object, .. } => {
                self.resolve_expression(object)?;
            }
            Expr::Grouping { expression } => {
                self.resolve_expression(expression)?;
            }
            Expr::Literal { .. } => {}
            Expr::Logical { left, right, .. } => {
                self.resolve_expression(left)?;
                self.resolve_expression(right)?;
            }
            Expr::Set { object, value, .. } => {
                self.resolve_expression(object)?;
                self.resolve_expression(value)?;
            }
            Expr::Super { keyword, .. } => {
                if self.current_class == ClassType::None {
                    return Err(ResolveError::from_token(
                        keyword,
                        "can't use 'super' outside of a class",
                    ));
                } else if self.current_class == ClassType::Class {
                    return Err(ResolveError::from_token(
                        keyword,
                        "can't use 'super' in a class without a super class",
                    ));
                }
                self.resolve_local(keyword);
            }
            Expr::This { keyword } => {
                if let ClassType::None = self.current_class {
                    return Err(ResolveError::from_token(
                        keyword,
                        "can't use 'this' outside of a class",
                    ));
                }
                self.resolve_local(keyword);
            }
            Expr::Unary { right, .. } => {
                self.resolve_expression(right)?;
            }
            Expr::Variable { name } => {
                if let Some(scope) = self.stack.last() {
                    if let Some(false) = scope.get(&name.lexeme) {
                        return Err(ResolveError::from_token(
                            name,
                            "can't read local variable in its own initializer",
                        ));
                    }
                }

                self.resolve_local(name);
            }
        };

        Ok(())
    }

    fn resolve_function(
        &mut self,
        parameters: &Vec<Token>,
        body: &Vec<Stmt>,
        function_type: FunctionType,
    ) -> Result<(), ResolveError> {
        let enclosing_type = self.current_function;
        self.current_function = function_type;
        self.begin_scope();
        for token in parameters {
            self.declare(token)?;
            self.define(token);
        }

        // body of a function is a block
        self.begin_scope();
        for statement in body {
            self.resolve_statement(statement)?;
        }
        self.end_scope();

        self.end_scope();
        self.current_function = enclosing_type;

        Ok(())
    }

    fn resolve_local(&mut self, name: &Token) {
        for (i, scope) in self.stack.iter().rev().enumerate() {
            if scope.contains_key(&name.lexeme) {
                self.locals.insert(name.clone(), i);
            }
        }
    }

    fn begin_scope(&mut self) {
        self.stack.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.stack
            .pop()
            .expect("ended a scope without a matching begin scope");
    }

    fn declare(&mut self, name: &Token) -> Result<(), ResolveError> {
        if let Some(scope) = self.stack.last_mut() {
            if scope.contains_key(&name.lexeme) {
                return Err(ResolveError::from_token(
                    name,
                    "already a variable with this name in this scope",
                ));
            }
            scope.insert(name.lexeme.clone(), false);
        }

        Ok(())
    }

    fn define(&mut self, name: impl AsRef<str>) {
        if let Some(scope) = self.stack.last_mut() {
            scope.insert(name.as_ref().to_string(), true);
        }
    }
}
