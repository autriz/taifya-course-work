use crate::{line_numbers::LineNumbers, parser::parse_module};

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

    let _ = ModuleAnalyzer::analyze(parsed.module, /*line_numbers, "./".into()*/);
}