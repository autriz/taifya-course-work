use std::collections::HashMap;

use crate::{analyzer::error::{Problems, Warning}, lexer::SrcSpan, object::{Value, ValueType}};

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Environment {
    pub store: HashMap<String, Value>,
    pub usages: HashMap<String, (SrcSpan, bool)>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            usages: HashMap::new()
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

    // pub fn set(&mut self, name: String, value: Value) -> Result<(), EvalError> {
    //     let var = self.store.get_mut(&name);

    //     match var {
    //         Some(var) => {
    //             if var._type() != value._type() {
    //                 /* error InvalidValueType */
    //                 panic!();
    //             }

    //             *var = value
    //         },
    //         None => panic!() /* error NotDeclaredVariable */
    //     }

    //     Ok(())
    // }

    pub fn set(&mut self, name: String, value: Value) {
        let var = self.store.get_mut(&name).unwrap();

        *var = value;
    }

    pub fn init_usage(
        &mut self,
        name: String,
        location: SrcSpan,
        problems: &mut Problems
    ) {
        match self.usages
            .insert(name.clone(), (location, false)) 
        {
            Some((location, false)) => {
                let mut unused = HashMap::with_capacity(1);
                let _ = unused.insert(name, (location, false));
                self.handle_unused(unused, problems);
            },
            _ => {}
        }
    }

    pub fn increment_usage(
        &mut self,
        name: &String
    ) {
        if let Some((_, used)) = self.usages.get_mut(name) {
            *used = true;
        }
    }

    pub fn convert_unused_to_warnings(&mut self, problems: &mut Problems) {
        let unused = HashMap::from_iter(self.usages.drain());

        self.handle_unused(unused, problems);
    }

    pub fn handle_unused(
        &mut self, 
        unused: HashMap<String, (SrcSpan, bool)>, 
        problems: &mut Problems
    ) {
        let unused = unused.into_iter().filter(|(_, (_, used))| !used);

        for (_, (location, _)) in unused {
            problems.warning(Warning::UnusedVariable { location })
        }
    }
}