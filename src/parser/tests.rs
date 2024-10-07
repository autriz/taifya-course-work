use crate::{
    ast::Statement, 
    lexer::Lexer, 
    parser::{ParseError, Parser}
};

use super::Parse;

#[test]
fn test_declarations() -> Result<(), ParseError> {
    let input =  r#"
        var a, e: $
        var b: !
        var c: %
        var d: @
    "#;

    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);

    let parsed = parser.parse()?;

    for stmt in parsed.module.statements {
        println!("{}", stmt);
    }

    for err in parser.lex_errors.iter() {
        println!("{:?}", err);
    }

    Ok(())
}

#[test]
fn test_blocks() -> Result<(), ParseError> {
    let input =  r#"
        begin
            var a: $;

            a := true;

            begin
                if (a == true) a;
                a := false
            end;

            begin
                if (a != true) a;
                a := true
            end
        end
    "#;

    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);

    let parsed = parser.parse()?;

    for stmt in parsed.module.statements {
        println!("{}", stmt);
    }

    Ok(())
}

#[test]
fn test_conditionals() -> Result<(), ParseError> {
    let input = r#"
        if (a == b) b else a
        if (a == b) begin a end else b
        if (a == b) a else begin b end
        if (a == b) begin a end else begin b end

        while (a > 5) a := a + 1
        while (a > 10) begin a := a + 1 end

        for i := 0 to 10 i next
        for i := 0 to 10 begin i end next

        for i := 0 to 10 step 2 i next
        for i := 0 to 10 step 2 begin i end next
    "#;

    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);

    let parsed = parser.parse()?;

    for stmt in parsed.module.statements {
        println!("{}", stmt);
    }

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

    for stmt in parsed.module.statements {
        println!("{}", stmt);
    }

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

    println!("{}", parsed.module.statements[0]);

    Ok(())
}