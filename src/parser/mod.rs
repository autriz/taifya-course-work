mod error;
pub use error::{ ParseError, ParseErrorType };

#[cfg(test)]
mod tests;

use crate::{
    ast::{
        Expression, IdentifierType, Module, Parsed, Statement
    }, 
    lexer::{
        Lexer, LexicalError, Spanned, SrcSpan
    }, 
    token::Token
};

pub trait Parse
    where Self: Sized,
{
    fn parse(
        parser: &mut Parser, 
        precedence: Option<Precedence>
    ) -> Result<Self, ParseError>;
}

pub trait InfixParse
    where Self: Sized,
{
    fn parse(
        parser: &mut Parser, 
        left: Expression, 
        precedence: Option<Precedence>
    ) -> Result<Self, ParseError>;
}

pub struct Parser {
    pub current_token: Option<Spanned>,
    pub next_token: Option<Spanned>,
    pub comments: Vec<SrcSpan>,
    pub lex_errors: Vec<LexicalError>,

    pub is_parsing_program: bool,
    lexer: Lexer,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            current_token: None,
            next_token: None,
            comments: vec![],
            lex_errors: vec![],
            is_parsing_program: false,
            lexer,
        };

        parser.step();
        parser.step();

        parser
    }

    pub fn step(&mut self) {
        let _ = self.next_token();
    }

    pub fn next_token(&mut self) -> Option<Spanned> {
        let t = self.current_token.take();
        let mut next = None;

        loop {
            match self.lexer.next() {
                Some(Ok((start, Token::Comment, end))) => {
                    self.comments.push(SrcSpan { start, end })
                },
                Some(Err(err)) => {
                    self.lex_errors.push(err);

                    break;
                },
                Some(Ok(tok)) => {
                    next = Some(tok);

                    break;
                },
                None => {
                    break;
                }
            }
        }

        self.current_token = self.next_token.take();
        self.next_token = next.take();

        t
    }

    pub fn current_precedence(&self) -> Precedence {
        match &self.current_token {
            Some((_, token, _)) => Precedence::from(token),
            None => Precedence::Lowest
        }
    }

    pub fn next_precedence(&self) -> Precedence {
        match &self.next_token {
            Some((_, token, _)) => Precedence::from(token),
            None => Precedence::Lowest
        }
    }

    pub fn parse(&mut self) -> Result<Parsed, ParseError> {
        // determine if program starts with 'begin' and ends with 'end'
        // check if begin exists at the start of a file
        let mut statements = vec![];

        while self.current_token.as_ref()
            .is_some_and(|(_, token, _)| *token != Token::Eof) 
        {
            statements.push(Statement::parse(self, None)?);

            // println!("in parse {:?}, {:?}", self.current_token, self.next_token);
            // self.step();
        }

        // check if end exists at the end of a file
        let module = Module {
            name: "".into(),
            statements,
        };

        Ok(Parsed {
            module,
            comments: Default::default()
        })
    }

    pub fn expect_one(&mut self, token: Token) -> Result<(u32, u32), ParseError> {
        match self.current_token.take() {
            Some((start, tok, end)) if tok == token => {
                self.step();
                Ok((start, end))
            },
            Some(t) => {
                let (start, tok, end) = t.clone();
                self.current_token = Some(t);

                parse_error(
                    ParseErrorType::UnexpectedToken {
                        token: tok,
                        expected: token,
                    },
                    SrcSpan { start, end }
                )
            },
            None => {
                self.current_token = None;

                parse_error(
                    ParseErrorType::UnexpectedEof,
                    SrcSpan { start: 0, end: 0 }
                )
            }
        }
    }

    pub fn expect_ident(&mut self) -> Result<(u32, String, u32), ParseError> {
        let span = self.next_token();

        match span {
            Some((start, tok, end)) => match tok {
                Token::Ident(ident) => Ok((start, ident, end)),
                _ if tok.is_reserved_word() => parse_error(
                    ParseErrorType::UnexpectedReservedWord,
                    SrcSpan { start, end }
                ),
                _ => parse_error(
                    ParseErrorType::ExpectedIdent, 
                    SrcSpan { start, end }
                )
            },
            None => parse_error(
                ParseErrorType::UnexpectedEof, 
                SrcSpan { start: 0, end: 0 }
            )
        }
    }

    pub fn parse_type_annotation(&mut self, start_token: Token) -> Result<(u32, IdentifierType, u32), ParseError> {
        let (start, end) = self.expect_one(start_token)?;

        match self.current_token.take() {
            Some((start, token, end)) if token.is_variable_type() => {
                self.step();
                Ok((start, IdentifierType::from(token), end))
            },
            tok => {
                self.current_token = tok;

                parse_error(
                    ParseErrorType::ExpectedType,
                    SrcSpan { start, end }
                )
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Assign,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix
}

impl From<&Token> for Precedence {
    fn from(value: &Token) -> Self {
        match value {
            Token::Equal | Token::NotEqual => Self::Equals,
            Token::LessThan | Token::GreaterThan | 
            Token::LessThanOrEqual | Token::GreaterThanOrEqual => Self::LessGreater,
            Token::Plus | Token::Minus => Self::Sum,
            Token::Slash | Token::Asterisk => Self::Product,
            Token::Assign => Self::Assign,
            _ => Self::Lowest,
        }
    }
}

pub fn parse_error<T>(error: ParseErrorType, span: SrcSpan) -> Result<T, ParseError> {
    Err(ParseError { error, span })
}