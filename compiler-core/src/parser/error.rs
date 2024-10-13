use crate::{lexer::{LexicalError, SrcSpan}, token::Token};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseErrorType {
    ExpectedIdent,
    ExpectedOperator,
    UnexpectedSemicolonBeforeEnd,
    UnexpectedEof,
    UnexpectedToken {
        token: Token,
        expected: Vec<String>,
    },
    ExpectedType,
    ExpectedValue,
    MissingSemicolon,
    LexError { error: LexicalError },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub error: ParseErrorType,
    pub span: SrcSpan
}

impl ParseError {
    pub fn details(&self) -> (&'static str, Vec<String>) {
        match &self.error {
            ParseErrorType::ExpectedIdent => ("Expected identifier", vec![]),
            ParseErrorType::ExpectedOperator => ("Expected operator", vec![]),
            ParseErrorType::UnexpectedSemicolonBeforeEnd => ("Unexpected semicolon before `end`", vec![]),
            ParseErrorType::ExpectedType => ("Expected type", vec![]),
            ParseErrorType::ExpectedValue => ("Expected value after `:=`", vec![]),
            // ParseErrorType::UnexpectedReservedWord => ("Unexpected reserved word", vec![]),
            ParseErrorType::UnexpectedToken { token, expected } => {
                let found = match token {
                    Token::Int(_) => "an Int".to_string(),
                    Token::Float(_) => "a Float".to_string(),
                    Token::String(_) => "a String".to_string(),
                    Token::Ident(_) => "an Identifier".to_string(),
                    _ if token.is_reserved_word() => format!("the keyword `{}`", token.as_literal()),
                    _ => format!("`{}`", token.as_literal())
                };

                let expected = expected.iter()
                    .map(|token| token.to_owned())
                    .collect::<Vec<String>>();

                let messages = std::iter::once(format!("Found {found}, expected one of: "))
                    .chain(expected.iter().map(|s| format!("- {s}")))
                    .collect();

                ("Not expected this", messages)
            },
            ParseErrorType::UnexpectedEof => ("Unexpected end of file", vec![]),
            ParseErrorType::MissingSemicolon => ("Missing semicolon", vec![]),
            ParseErrorType::LexError { error } => error.details()
        }
    }
}