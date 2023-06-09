use crate::interpreter::LoxValue;
use std::{
  collections::{hash_map::Entry, HashMap},
  sync::{Arc, Mutex},
};

#[derive(Default)]
pub struct Environ {
  pub values: HashMap<String, LoxValue>,
  pub enclosing: Option<Arc<Mutex<Environ>>>,
}

impl Environ {
  pub fn new(enclosing: Option<Arc<Mutex<Environ>>>) -> Self {
    Environ {
      values: HashMap::new(),
      enclosing,
    }
  }

  pub fn define(&mut self, name: String, val: LoxValue) {
    self.values.insert(name, val);
  }

  pub fn assign(&mut self, name: String, val: LoxValue) {
    if let Entry::Occupied(mut e) = self.values.entry(name.to_string()) {
      e.insert(val);
      return;
    }

    if let Some(ref mut enclosing) = self.enclosing {
      enclosing.lock().unwrap().assign(name, val);
    }
  }

  pub fn get(&self, name: String) -> LoxValue {
    if let Some(val) = self.values.get(&name) {
      return val.clone();
    }

    if let Some(ref enclosing) = self.enclosing {
      return enclosing.lock().unwrap().get(name);
    }

    panic!("Undefined variable");
  }
}
