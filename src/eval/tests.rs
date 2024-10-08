use std::{cell::RefCell, rc::Rc};

use crate::{lexer::Lexer, object::environment::Environment, parser::Parser};

use super::{eval, EvalError};

#[test]
fn test_program() {
    let input = r#"
        begin
            var a, b, c: %; d, e, f: !;
        end
    "#;

    let parsed = Parser::new(Lexer::new(input.to_string())).parse().unwrap();

    let env = Rc::new(RefCell::new(Environment::new()));

    let obj = eval(parsed, env.clone());

    println!("{env:?}");
}