use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::types::LoxValue;

#[derive(Debug)]
pub struct Environment {
    values: HashMap<String, Rc<LoxValue>>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Self {
        Environment {
            values: HashMap::new(),
            enclosing,
        }
    }

    pub fn define(&mut self, name: impl AsRef<str>, value: LoxValue) {
        self.values
            .insert(name.as_ref().to_string(), Rc::new(value));
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<Rc<LoxValue>> {
        self.get_at(0, name)
    }

    pub fn get_at(&self, distance: usize, name: impl AsRef<str>) -> Option<Rc<LoxValue>> {
        if distance == 0 {
            return self.values.get(name.as_ref()).cloned();
        }

        match &self.enclosing {
            Some(e) => e.borrow().get_at(distance - 1, name),
            None => None,
        }
    }

    pub fn assign(&mut self, name: impl AsRef<str>, value: LoxValue) -> Option<()> {
        self.assign_at(0, name, value)
    }

    pub fn assign_at(
        &mut self,
        distance: usize,
        name: impl AsRef<str>,
        value: LoxValue,
    ) -> Option<()> {
        if distance == 0 {
            self.values
                .insert(name.as_ref().to_string(), Rc::new(value));
            return Some(());
        }

        match &self.enclosing {
            Some(e) => e.borrow_mut().assign_at(distance - 1, name, value),
            None => None,
        }
    }
}
