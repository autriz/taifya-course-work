use crate::{lexer::{LexicalError, SrcSpan}, token::Token};

#[derive(Debug, PartialEq, Eq)]
pub enum ParseErrorType {
    ExpectedIdent,
    ExpectedPrimitiveOrInfix,
    ExpectedOperator,
    // UnexpectedReservedWord,
    UnexpectedEof,
    UnexpectedToken {
        token: Token,
        expected: Token,
    },
    ExpectedType,
    ExpectedInfix,
    LexError { error: LexicalError },
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError {
    pub error: ParseErrorType,
    pub span: SrcSpan
}

impl ParseError {
    pub fn details(&self) -> (&'static str, Vec<String>) {
        match &self.error {
            ParseErrorType::ExpectedIdent => ("Expected identifier", vec![]),
            ParseErrorType::ExpectedPrimitiveOrInfix => ("Expected primitive or nested infix", vec![]),
            ParseErrorType::ExpectedOperator => ("Expected operator", vec![]),
            ParseErrorType::ExpectedType => ("Expected type", vec![]),
            ParseErrorType::ExpectedInfix => ("Expected expression", vec![]),
            // ParseErrorType::UnexpectedReservedWord => ("Unexpected reserved word", vec![]),
            ParseErrorType::UnexpectedToken { token, expected } => {
                let found = match token {
                    Token::Int(_) => "an Int".to_string(),
                    Token::Float(_) => "a Float".to_string(),
                    Token::Hexadecimal(_) => "a Hex".to_string(),
                    Token::Octal(_) => "an Octal".to_string(),
                    Token::Binary(_) => "a Binary".to_string(),
                    Token::String(_) => "a String".to_string(),
                    Token::Ident(_) => "an Identifier".to_string(),
                    _ if token.is_reserved_word() => format!("the keyword `{}`", token.as_literal()),
                    _ => token.as_literal()
                };

                let messages = std::iter::once(
                    format!("Found {found}, expected `{}`", expected.as_literal())
                ).collect();

                ("Not expected this", messages)
            },
            ParseErrorType::UnexpectedEof => ("Unexpected end of file", vec![]),
            ParseErrorType::LexError { error } => error.details()
        }
    }
}