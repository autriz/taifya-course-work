use crate::{lexer::SrcSpan, token::Token};

#[derive(Debug, PartialEq, Eq)]
pub enum ParseErrorType {
    ExpectedIdent,
    ExpectedOperator,
    UnexpectedReservedWord,
    UnexpectedEof,
    UnexpectedToken {
        token: Token,
        expected: Token,
    },
    ExpectedType,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError {
    pub error: ParseErrorType,
    pub span: SrcSpan
}