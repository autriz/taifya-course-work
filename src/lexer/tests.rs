use crate::token::Token;
use super::{Lexer, LexicalError, LexicalErrorType};

#[test]
fn test_numbers() -> std::result::Result<(), LexicalError> {
    let input = r#"
        10;
        125d;
        196D;
        01010b;
        1001B;
        5424o;
        12367O;
        1A3h;
        1E5H;
        10e5;
        10.4E5;
        .0e-5;
        1.5;
    "#;

    let mut lexer = Lexer::new(input.to_string());

    let tokens = vec![
        Token::Int(String::from("10")),
        Token::Semicolon,
        Token::Int(String::from("125")),
        Token::Semicolon,
        Token::Int(String::from("196")),
        Token::Semicolon,
        Token::Binary(String::from("01010")),
        Token::Semicolon,
        Token::Binary(String::from("1001")),
        Token::Semicolon,
        Token::Octal(String::from("5424")),
        Token::Semicolon,
        Token::Octal(String::from("12367")),
        Token::Semicolon,
        Token::Hexadecimal(String::from("1A3")),
        Token::Semicolon,
        Token::Hexadecimal(String::from("1E5")),
        Token::Semicolon,
        Token::Float(String::from("10e5")),
        Token::Semicolon,
        Token::Float(String::from("10.4E5")),
        Token::Semicolon,
        Token::Float(String::from(".0e-5")),
        Token::Semicolon,
        Token::Float(String::from("1.5")),
        Token::Semicolon,
    ];

    for (idx, token) in tokens.iter().enumerate() {
        let (_, next_token, _) = match lexer.next_token() {
            Ok(next_token) => next_token,
            Err(err) => {
                println!("stopped at {token:?} ({idx})");
                panic!("{err:?}")
            }
        };

        assert_eq!(
            *token, next_token, 
            "Next token does not match expected token ({:?}, {:?}) at {}", 
            next_token, token, idx
        );
    }

    Ok(())
}

#[test]
fn test_invalid_numbers() -> std::result::Result<(), LexicalError> {
    let input = r#"
        123b
        789o
        1A3
        1B6.F4h
        435.54o
        0101000.101b
        1.2.3
        10.e5
        .0e
    "#;

    let mut lexer = Lexer::new(input.to_string());

    let fails = vec![
        LexicalErrorType::DigitOutOfRadix,
        LexicalErrorType::DigitOutOfRadix,
        LexicalErrorType::DigitOutOfRadix,
        LexicalErrorType::UnsupportedFloatingPoint,
        LexicalErrorType::UnsupportedFloatingPoint,
        LexicalErrorType::UnsupportedFloatingPoint,
        LexicalErrorType::MultipleFloatingPoints,
        LexicalErrorType::MissingNumberBeforeExponent,
        LexicalErrorType::MissingDigitsAfterExponent
    ];

    for (idx, fail) in fails.iter().enumerate() {
        let err = match lexer.next_token() {
            Err(err) => err,
            Ok(value) => {
                panic!("Stopped at {fail:?} ({idx}). Expected Err but got Ok({value:?})");
            }
        };

        assert_eq!(
            *fail, err.error, 
            "Next token does not match expected token ({:?}, {:?}) at {}", 
            fail, err.error, idx
        );
    }

    Ok(())
}

#[test]
fn test_input() -> std::result::Result<(), LexicalError> {
    let input = r#"begin
var a: %;
var b: @;
var c: $;
var d: !;

a := 10;
b := "hello, world!";
c := false;
d := 10.54;

(*
    multiline
    comment
*)

a + d;
10 - 5;
.5 / 5.0;
2.104e5 * 2;
c != true;
c == false;

for i := 5 to 10 do
    begin
        writeln i;
    end
next;

if (!c) writeln "c is false"
else writeln "c is true";

while (a > 5) a := a - 1;

end
    "#;

    let mut lexer = Lexer::new(input.to_string());

    let tokens = vec![
        Token::Begin,

        Token::Var,
        Token::Ident(String::from("a")),
        Token::Colon,
        Token::Percent,
        Token::Semicolon,

        Token::Var,
        Token::Ident(String::from("b")),
        Token::Colon,
        Token::At,
        Token::Semicolon,

        Token::Var,
        Token::Ident(String::from("c")),
        Token::Colon,
        Token::Dollar,
        Token::Semicolon,

        Token::Var,
        Token::Ident(String::from("d")),
        Token::Colon,
        Token::Bang,
        Token::Semicolon,

        Token::Ident(String::from("a")),
        Token::Assign,
        Token::Int(String::from("10")),
        Token::Semicolon,

        Token::Ident(String::from("b")),
        Token::Assign,
        Token::String(String::from("hello, world!")),
        Token::Semicolon,

        Token::Ident(String::from("c")),
        Token::Assign,
        Token::False,
        Token::Semicolon,

        Token::Ident(String::from("d")),
        Token::Assign,
        Token::Float(String::from("10.54")),
        Token::Semicolon,

        Token::Comment(String::from("\n    multiline\n    comment\n")),

        Token::Ident(String::from("a")),
        Token::Plus,
        Token::Ident(String::from("d")),
        Token::Semicolon,

        Token::Int(String::from("10")),
        Token::Minus,
        Token::Int(String::from("5")),
        Token::Semicolon,

        Token::Float(String::from(".5")),
        Token::Slash,
        Token::Float(String::from("5.0")),
        Token::Semicolon,

        Token::Float(String::from("2.104e5")),
        Token::Asterisk,
        Token::Int(String::from("2")),
        Token::Semicolon,

        Token::Ident(String::from("c")),
        Token::NotEqual,
        Token::True,
        Token::Semicolon,

        Token::Ident(String::from("c")),
        Token::Equal,
        Token::False,
        Token::Semicolon,

        Token::For,
        Token::Ident(String::from("i")),
        Token::Assign,
        Token::Int(String::from("5")),
        Token::To,
        Token::Int(String::from("10")),
        Token::Do,
        Token::Begin,
        Token::Writeln,
        Token::Ident(String::from("i")),
        Token::Semicolon,
        Token::End,
        Token::Next,
        Token::Semicolon,

        Token::If,
        Token::LParen,
        Token::Bang,
        Token::Ident(String::from("c")),
        Token::RParen,
        Token::Writeln,
        Token::String(String::from("c is false")),
        Token::Else,
        Token::Writeln,
        Token::String(String::from("c is true")),
        Token::Semicolon,

        Token::While,
        Token::LParen,
        Token::Ident(String::from("a")),
        Token::GreaterThan,
        Token::Int(String::from("5")),
        Token::RParen,
        Token::Ident(String::from("a")),
        Token::Assign,
        Token::Ident(String::from("a")),
        Token::Minus,
        Token::Int(String::from("1")),
        Token::Semicolon,

        Token::End,
        Token::Eof,
    ];

    for (idx, token) in tokens.iter().enumerate() {
        let (_, next_token, _) = match lexer.next_token() {
            Ok(next_token) => next_token,
            Err(err) => {
                println!("stopped at {token:?} ({idx})");
                panic!("{err:?}")
            }
        };

        println!("{token:?}, {next_token:?}");

        assert_eq!(
            *token, next_token, 
            "Next token does not match expected token ({:?}, {:?}) at {}", 
            next_token, token, idx
        );
    }

    Ok(())
}