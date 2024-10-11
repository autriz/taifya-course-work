use std::{path::PathBuf, rc::Rc};

use crate::{line_numbers::LineNumbers, parser::parse_module, warning::{NullWarningEmitterIO, TypeWarningEmitter, WarningEmitter}};

use super::ModuleAnalyzer;

#[test]
fn test_module() {
    let input = r#"
        begin
            var a, b, c: %;;

            a := 5;
            b := 10
        end
    "#;

    let parsed = parse_module(input).unwrap();

    let line_numbers = LineNumbers::new(input);

    println!("{line_numbers:?}");

    let warnings = Rc::new(NullWarningEmitterIO);

    let emitter = &TypeWarningEmitter::new(
        PathBuf::new(), 
        input.to_string(), 
        WarningEmitter::new(warnings.clone())
    );

    let _ = ModuleAnalyzer::analyze(parsed.module, &emitter/*line_numbers, "./".into()*/);
}