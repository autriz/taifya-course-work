use std::collections::HashMap;

use crate::eval::EvalError;

use super::{Value, ValueType};

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Environment {
    pub store: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, name: &String) -> Option<&Value> {
        self.store.get(name)
    }

    pub fn declare(&mut self, name: String, value_type: ValueType) {
        let default_value =  match value_type {
            ValueType::Integer => Value::Integer { value: 0 },
            ValueType::Float => Value::Float { value: 0.0 },
            ValueType::String => Value::String { value: "".to_string() },
            ValueType::Boolean => Value::Boolean { value: false },
        };

        self.store.insert(name.clone(), default_value);
    }

    pub fn set(&mut self, name: String, value: Value) -> Result<(), EvalError> {
        let var = self.store.get_mut(&name);

        match var {
            Some(var) => {
                if var._type() != value._type() {
                    /* error InvalidValueType */
                    panic!();
                }

                *var = value
            },
            None => panic!() /* error NotDeclaredVariable */
        }

        Ok(())
    }
}