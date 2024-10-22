use crate::{
    lexer::prelude::Lexer, 
    parser::prelude::{parse_module, ParseError, Parser}
};

#[test]
fn test_declarations() -> Result<(), ParseError> {
    let input =  r#"
        begin
            var a, e: $;
                c, d: !;;
            var d: @;
        end
    "#;

    let parsed = parse_module(input)?;

    println!("{}", parsed.module.program);

    Ok(())
}

#[test]
fn test_infixes() -> Result<(), ParseError> {
    let input = r#"
        begin
            var a, b: %;;

            if (false && true) writeln true;
            if (true || false) writeln 1;

            begin
                a := 5 + 5;
                b := 10 - 10;
                a := 6 * 10;
                b := 10 / 6
            end
        end
    "#;

    let parsed = parse_module(input)?;

    println!("{}", parsed.module.program);

    Ok(())
}

#[test]
fn test_prefix() -> Result<(), ParseError> {
    let input = r#"
        begin
            if (!!!false) writeln true;
            if (!true == !!false) writeln false
        end
    "#;

    let parsed = parse_module(input)?;

    println!("{}", parsed.module.program);

    Ok(())
}

#[test]
fn test_blocks() -> Result<(), ParseError> {
    let input =  r#"
        begin
            var a: $;;

            a := true;

            begin
                if (a == true) writeln a;
                a := false
            end;

            begin
                if (a != true) writeln a;
                a := true
            end
        end
    "#;

    let parsed = parse_module(input)?;

    println!("{}", parsed.module.program);

    Ok(())
}

#[test]
fn test_conditionals() -> Result<(), ParseError> {
    let input = r#"
        begin
            if (a == b) writeln b else writeln a;
            if (a == b) begin writeln a end else writeln b;
            if (a == b) writeln a else begin writeln b end;
            if (a == b) begin writeln a end else begin writeln b end;

            while (a > 5) a := a + 1;
            while (a > 10) begin a := a + 1 end;

            for i := 0 to 10 writeln i next;
            for i := 0 to 10 begin writeln i end next;

            for i := 0 to 10 step 2 writeln i next;
            for i := 0 to 10 step 2 begin writeln i end next
        end
    "#;

    let parsed = parse_module(input)?;

    println!("{}", parsed.module.program);

    Ok(())
}

#[test]
fn test_values() -> Result<(), ParseError> {
    let input = r#"
        begin
            var a, b, c, d: %;
                e, f, g: !;
                h, j: @;
                k, l: $;;

            a := 23;
            b := 10101b;
            c := 2345o;
            d := 1A5D9h;
            e := 10.15;
            f := 1e-5;
            g := 1.0e10;
            h := "hello";
            j := "world";
            k := true;
            l := false
        end
    "#;

    let parsed = parse_module(input)?;

    println!("{}", parsed.module.program);

    Ok(())
}

#[test]
fn test_program() -> Result<(), ParseError> {
    let input = r#"
        begin
            var a: $; b: !;;
            var c: !;;
            var
        end
    "#;

    let lexer = Lexer::new(input.char_indices().map(|(i, c)| (i as u32, c)));
    let mut parser = Parser::new(lexer);

    let parsed = parser.parse()?;

    println!("{}", parsed.module.program);

    Ok(())
}