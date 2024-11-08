use super::prelude::{Lexer, LexicalError, LexicalErrorType, Token};

#[test]
fn test_numbers() -> std::result::Result<(), LexicalError> {
    let input = r#"
        10
        125d
        196D
        01010b
        1001B
        5424o
        12367O
        1A3h
        1E5H
        10e5
        10.4E5
        .0e-5
        1.5
    "#;

    let mut lexer = Lexer::new(input.char_indices().map(|(i, c)| (i as u32, c)));

    let tokens = vec![
        Token::Int(10),
        Token::Int(125),
        Token::Int(196),
        Token::Int(10),
        Token::Int(9),
        Token::Int(2836),
        Token::Int(5367),
        Token::Int(419),
        Token::Int(485),
        Token::Float(1000000.0),
        Token::Float(1040000.0),
        Token::Float(0.0),
        Token::Float(1.5),
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
fn test_long_numbers() -> std::result::Result<(), LexicalError> {
    let input1 = "12345678909876543232345679434657905454754354605749746756535353453453234567890987654323234567943465790545475435460574974675653535345345323456789098765432323456794346579054547543546057497467565353534534532345678909876543232345679434657905454754354605749746756535353453453;";
    let input2 = "0DEABCFBCFADECFBDEACBFEACFBDACBFADECBFAEDCFBDECFBEDCFBADEACFBEDBCFEADBCFAEDBCFDAECBFDAECFBADBECFDAECFBADCEFBAEDCFBBDCFAECBFDEAFBCEDACFBDAECBFEDABCFEDACFBEDABCFDAEBCFADEBCFADECBFDECBFDEABCFAEDBCFEDACFBDEABCFAEDCFBCAEDFBBCFADECBFAEDCFBAEDBCFEDABCFAEDCFBADECFBAEDCFBAEDCFB;";
    
    let mut times1 = vec![];
    let mut times2 = vec![];

    for _ in 0..1000000 {
        let mut lexer1 = Lexer::new(input1.char_indices().map(|(i, c)| (i as u32, c)));
        
        let start = std::time::Instant::now();
        let _ = lexer1.next_token();
        times1.push((std::time::Instant::now() - start).as_nanos());
    }
    println!("1: {}ns", times1.iter().sum::<u128>() / times1.len() as u128);

    for _ in 0..1000000 {
        let mut lexer2 = Lexer::new(input2.char_indices().map(|(i, c)| (i as u32, c)));
        
        let start = std::time::Instant::now();
        let _ = lexer2.next_token();
        times2.push((std::time::Instant::now() - start).as_nanos());
    }
    println!("2: {}ns", times1.iter().sum::<u128>() / times1.len() as u128);


    Ok(())
}

#[test]
fn test_invalid_numbers() -> std::result::Result<(), LexicalError> {
    let input = r#"
        1.eh
        1e+5e
        123b
        789o
        1A3
        1B6.F4h
        435.54o
        0101000.101b
        1.2.
        10.e5
        .0e
    "#;

    let mut lexer = Lexer::new(input.char_indices().map(|(i, c)| (i as u32, c)));

    let fails = vec![
        LexicalErrorType::UnsupportedFloatingPoint,
        LexicalErrorType::DigitOutOfRadix,
        LexicalErrorType::DigitOutOfRadix,
        LexicalErrorType::DigitOutOfRadix,
        LexicalErrorType::DigitOutOfRadix,
        LexicalErrorType::UnsupportedFloatingPoint,
        LexicalErrorType::UnsupportedFloatingPoint,
        LexicalErrorType::UnsupportedFloatingPoint,
        LexicalErrorType::MultipleFloatingPoints,
        LexicalErrorType::MissingDigitBeforeExponent,
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
var a: %;;
var b: $;;
var c: !;;

a := 10;
b := false;
c := 10.54;

(*
    multiline
    comment
*)

a + c;
10 - 5;
.5 / 5.0;
2.104e5 * 2;
c != true;
c == false;

for i := 5 to 10
    begin
        writeln i;
    end
next;

if (!c) writeln "c is false"
else writeln "c is true";

while (a > 5) a := a - 1;

end
    "#;

    let mut lexer = Lexer::new(input.char_indices().map(|(i, c)| (i as u32, c)));

    let tokens = vec![
        Token::Begin,

        Token::Var,
        Token::Ident(String::from("a")),
        Token::Colon,
        Token::Percent,
        Token::Semicolon,
        Token::Semicolon,

        Token::Var,
        Token::Ident(String::from("b")),
        Token::Colon,
        Token::Dollar,
        Token::Semicolon,
        Token::Semicolon,

        Token::Var,
        Token::Ident(String::from("c")),
        Token::Colon,
        Token::Bang,
        Token::Semicolon,
        Token::Semicolon,

        Token::Ident(String::from("a")),
        Token::Assign,
        Token::Int(10),
        Token::Semicolon,

        Token::Ident(String::from("b")),
        Token::Assign,
        Token::False,
        Token::Semicolon,

        Token::Ident(String::from("c")),
        Token::Assign,
        Token::Float(10.54),
        Token::Semicolon,

        Token::Comment,

        Token::Ident(String::from("a")),
        Token::Plus,
        Token::Ident(String::from("c")),
        Token::Semicolon,

        Token::Int(10),
        Token::Minus,
        Token::Int(5),
        Token::Semicolon,

        Token::Float(0.5),
        Token::Slash,
        Token::Float(5.0),
        Token::Semicolon,

        Token::Float(210400.0),
        Token::Asterisk,
        Token::Int(2),
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
        Token::Int(5),
        Token::To,
        Token::Int(10),
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
        Token::Int(5),
        Token::RParen,
        Token::Ident(String::from("a")),
        Token::Assign,
        Token::Ident(String::from("a")),
        Token::Minus,
        Token::Int(1),
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