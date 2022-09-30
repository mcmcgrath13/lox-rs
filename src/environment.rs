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
        Environment {
            values: HashMap::new(),
            enclosing,
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

    pub fn assign(&mut self, name: &Token, value: LoxValue) -> Option<()> {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme.clone(), value);
            return Some(());
        }

        if let Some(e) = &self.enclosing {
            return e.borrow_mut().assign(name, value);
        }

        None
    }
}