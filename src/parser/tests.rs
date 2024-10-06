use crate::lexer::Lexer;

use super::{ParseError, Parser};

#[test]
fn test_vars() -> Result<(), ParseError> {
    let input =  r#"
        var a: $;
        var b: !;
        var c: %;
        var d: @;
    "#;

    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);

    let parsed = parser.parse()?;

    println!("{:?}", parsed.module.statements);

    Ok(())
}

#[test]
fn test_conditionals() -> Result<(), ParseError> {
    let input = r#"
        if (a == b) writeln b else writeln a
    "#;

    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);

    let parsed = parser.parse()?;

    println!("{:?}", parsed.module.statements);

    Ok(())
}

#[test]
fn test_values() -> Result<(), ParseError> {
    let input = r#"
        10.15;
        23;
        10101b;
        2345o;
        1A5D9h;
        "hello";
    "#;

    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);

    let parsed = parser.parse()?;

    println!("{:?}", parsed.module.statements);

    Ok(())
}

#[test]
fn test_program() -> Result<(), ParseError> {
    let input = r#"
        begin
            var a: $;
            var b: !
        end
    "#;

    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);

    let parsed = parser.parse()?;

    println!("{:?}", parsed.module.statements);

    Ok(())
}