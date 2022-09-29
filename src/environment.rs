use std::collections::HashMap;

use crate::token::Token;
use crate::interpreter::LoxValue;

#[derive(Debug)]
pub struct Environment {
    values: HashMap<String, LoxValue>,
    parent: Option<Box<Environment>>,
}

impl Environment {
    pub fn new(parent: Option<Environment>) -> Self {
        match parent {
            Some(e) => Environment {
                values: HashMap::new(),
                parent: Some(Box::new(e)),
            },
            None => Environment {
                values: HashMap::new(),
                parent: None,
            }
        }

    }

    pub fn define(&mut self, name: &Token, value: LoxValue) {
        self.values.insert(name.lexeme.to_string(), value);
    }

    pub fn get(&self, name: &Token) -> Option<LoxValue> {
        match self.values.get(&name.lexeme.to_string()) {
            Some(v) => Some(v.clone()),
            None => {
                match &self.parent {
                    Some(e) => e.get(name),
                    None => None
                }
            }
        }
    }
}
