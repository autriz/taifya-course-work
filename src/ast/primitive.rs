use std::fmt::Display;

use crate::{lexer::SrcSpan, parser::Parse, token::Token};

#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    Int {
        value: i64,
        location: SrcSpan
    },
    Float {
        value: f64,
        location: SrcSpan
    },
    Hex {
        value: i64,
        location: SrcSpan
    },
    Octal {
        value: i64,
        location: SrcSpan
    },
    Bin {
        value: i64,
        location: SrcSpan
    },
    String {
        value: String,
        location: SrcSpan
    },
    Bool {
        value: bool,
        location: SrcSpan
    }
}

impl Parse for Primitive {
    fn parse(
        parser: &mut crate::parser::Parser, 
        _precedence: Option<crate::parser::Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let span = parser.next_token();

        let primitive = match span {
            Some((start, token, end)) => match token {
                Token::Int(value) => {
                    let parsed = value.parse();

                    match parsed {
                        Ok(value) => Self::Int { 
                            value, 
                            location: SrcSpan { start, end } 
                        },
                        Err(_err) => {
                            todo!("parse error");
                        }
                    }
                },
                Token::Float(value) => {
                    let parsed = value.parse();

                    match parsed {
                        Ok(value) => Self::Float { 
                            value, 
                            location: SrcSpan { start, end } 
                        },
                        Err(_err) => {
                            todo!("parse error");
                        }
                    }
                },
                Token::Hexadecimal(value) => {
                    let parsed = i64::from_str_radix(&value, 16);

                    match parsed {
                        Ok(value) => Self::Hex { 
                            value, 
                            location: SrcSpan { start, end } 
                        },
                        Err(_err) => {
                            todo!("parse error");
                        }
                    }
                },
                Token::Octal(value) => {
                    let parsed = i64::from_str_radix(&value, 8);

                    match parsed {
                        Ok(value) => Self::Octal { value, location: SrcSpan { start, end } },
                        Err(_err) => {
                            todo!("parse error");
                        }
                    }
                },
                Token::Binary(value) => {
                    let parsed = i64::from_str_radix(&value, 2);

                    match parsed {
                        Ok(value) => Self::Bin { value, location: SrcSpan { start, end } },
                        Err(_err) => {
                            todo!("parse error");
                        }
                    }
                },
                Token::String(value) => {
                    Self::String { value, location: SrcSpan { start, end } }
                },
                Token::True => Self::Bool { value: true, location: SrcSpan { start, end } },
                Token::False => Self::Bool { value: false, location: SrcSpan { start, end } },
                _ => unreachable!("Primitive parser should not reach this place")
            },
            None => unreachable!("Primitive parser should not reach this place")
        };

        Ok(primitive)
    }
}

impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int { value, .. } => write!(f, "{}", value),
            Self::Float { value, .. } => write!(f, "{}", value),
            Self::Hex { value, .. } => write!(f, "{:x}", value),
            Self::Octal { value, .. } => write!(f, "{:o}", value),
            Self::Bin { value, .. } => write!(f, "{:b}", value),
            Self::String { value, .. } => write!(f, "\"{}\"", value),
            Self::Bool { value, .. } => write!(f, "{}", value),
        }
    }
}

impl Primitive {
    pub fn location(&self) -> SrcSpan {
        match self {
            Self::Int { location, .. } |
            Self::Float { location, .. } |
            Self::Hex { location, .. } |
            Self::Octal { location, .. } |
            Self::Bin { location, .. } |
            Self::String { location, .. } |
            Self::Bool { location, .. } => *location
        }
    }
}