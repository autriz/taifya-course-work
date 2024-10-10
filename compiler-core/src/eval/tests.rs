use std::{cell::RefCell, rc::Rc};

use crate::{environment::Environment, parser::parse_module};

use super::eval;

#[test]
fn test_program() {
    let input = r#"
        begin
            var a, b, c: %; d, e, f: !;^

            
        end
    "#;

    let parsed = parse_module(input).unwrap();

    let env = Rc::new(RefCell::new(Environment::new()));

    let obj = eval(parsed, env.clone());

    println!("{env:?}");
}