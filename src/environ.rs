use crate::interpreter::LoxValue;
use rand::{self, distributions::Alphanumeric, Rng};
use std::{
  collections::{hash_map::Entry, HashMap},
  sync::{Arc, Mutex},
};

#[derive(Default, Debug)]
pub struct Environ {
  pub env_id: String,
  pub values: HashMap<String, LoxValue>,
  pub enclosing: Option<Arc<Mutex<Environ>>>,
}

impl Environ {
  pub fn new(enclosing: Option<Arc<Mutex<Environ>>>) -> Self {
    Environ {
      env_id: rand::thread_rng()
        .sample_iter(&Alphanumeric)
        .take(7)
        .map(char::from)
        .collect(),
      values: HashMap::new(),
      enclosing,
    }
  }

  pub fn id(&mut self) -> String {
    self.env_id.to_string()
  }
  // Define variable in current scope
  pub fn define(&mut self, name: String, val: LoxValue) {
    self.values.insert(name, val);
  }

  // Assign value to variable defined in the smallest scope.
  // Search from the current scope, and move out to global.
  pub fn assign(&mut self, name: String, val: LoxValue) {
    if let Entry::Occupied(mut e) = self.values.entry(name.to_string()) {
      e.insert(val);
      return;
    }

    if let Some(ref mut enclosing) = self.enclosing {
      enclosing.lock().unwrap().assign(name, val);
    }
  }

  // Search for variable starting in smallest scope
  pub fn get(&self, name: String) -> LoxValue {
    if let Some(val) = self.values.get(&name) {
      return val.clone();
    }

    if let Some(ref enclosing) = self.enclosing {
      return enclosing.lock().unwrap().get(name);
    }

    panic!("Undefined variable! {}", name);
  }
}
