use std::collections::HashMap;

use crate::interpreter::LoxValue;
use crate::token::Token;

#[derive(Debug)]
pub struct Environment<'e> {
    values: HashMap<String, LoxValue>,
    pub enclosing: Option<&'e mut Environment<'e>>,
}

impl<'e> Environment<'e> {
    pub fn new(enclosing: Option<&'e mut Environment<'e>>) -> Self {
        match enclosing {
            Some(e) => Environment {
                values: HashMap::new(),
                enclosing: Some(e),
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
                Some(e) => e.get(name),
                None => None,
            },
        }
    }

    pub fn assign(&mut self, name: &Token, value: LoxValue) -> Option<LoxValue> {
        if self.values.contains_key(&name.lexeme) {
            return match self.values.insert(name.lexeme.clone(), value.clone()) {
                Some(v) => Some(v),
                None => match &mut self.enclosing {
                    Some(e) => e.assign(name, value),
                    None => None,
                },
            };
        }

        None
    }
}
