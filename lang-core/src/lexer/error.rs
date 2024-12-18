use crate::utils::prelude::SrcSpan;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexicalErrorType {
    UnrecognizedToken { tok: char },
    MissingDigitAfterPeriod,
    MissingDigitBeforeExponent,
    MissingDigitsAfterExponent,
    MultipleFloatingPoints,
    DigitOutOfRadix,
    UnsupportedFloatingPoint,
    UnexpectedStringEnd,
    MissingCommentEnd,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LexicalError {
    pub error: LexicalErrorType,
    pub location: SrcSpan
}

impl LexicalError {
    pub fn details(&self) -> (&'static str, Vec<String>) {
        match self.error {
            LexicalErrorType::DigitOutOfRadix => {
                ("This digit is too big for the specified radix", vec![])
            },
            LexicalErrorType::MissingDigitAfterPeriod => {
                ("Missing a digit after period", vec![])
            },
            LexicalErrorType::MissingDigitBeforeExponent => {
                ("Missing a digit before exponent", vec![])
            },
            LexicalErrorType::MissingDigitsAfterExponent => {
                ("Missing digits after exponent", vec![])
            },
            LexicalErrorType::MultipleFloatingPoints => {
                ("Found multiple periods in float number", vec![])
            },
            LexicalErrorType::UnrecognizedToken { .. } => {
                ("I can't figure out what to do with this character", vec![])
            },
            LexicalErrorType::UnsupportedFloatingPoint => {
                ("This number doesn't support floating point variants", vec![])
            },
            LexicalErrorType::UnexpectedStringEnd => {
                (r#"Missing trailing `"` symbol to terminate the string literal"#, vec![])
            },
            LexicalErrorType::MissingCommentEnd => {
                ("Missing trailing `*)` symbols to terminate comment", vec![])
            }
        }
    }
}