use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::interpreter::LoxValue;
use crate::token::Token;

#[derive(Debug)]
pub struct Environment {
    values: HashMap<String, LoxValue>,
    pub enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Self {
        match enclosing {
            Some(e) => Environment {
                values: HashMap::new(),
                enclosing: Some(Rc::clone(&e)),
            },
            None => Environment {
                values: HashMap::new(),
                enclosing: None,
            },
        }
    }

    pub fn define(&mut self, name: &Token, value: LoxValue) {
        self.values.insert(name.lexeme.clone(), value);
    }

    pub fn get(&self, name: &Token) -> Option<LoxValue> {
        match self.values.get(&name.lexeme) {
            Some(v) => Some(v.clone()),
            None => match &self.enclosing {
                Some(e) => e.borrow().get(name),
                None => None,
            },
        }
    }

    pub fn assign(&mut self, name: &Token, value: LoxValue) -> Option<LoxValue> {
        if self.values.contains_key(&name.lexeme) {
            return match self.values.insert(name.lexeme.clone(), value.clone()) {
                Some(v) => Some(v),
                None => match &mut self.enclosing {
                    Some(e) => e.borrow_mut().assign(name, value),
                    None => None,
                },
            };
        }

        None
    }
}
