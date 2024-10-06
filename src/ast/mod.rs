use std::fmt::Display;

use crate::{lexer::SrcSpan, parser::{parse_error, InfixParse, Parse, ParseErrorType, Precedence}, token::Token};

#[derive(Debug)]
pub struct Parsed {
    pub module: Module,
    pub comments: Vec<SrcSpan>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: String,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Program(Program),
    Declaration(Declaration), // var a: $;
    Block(Block), // begin a + b end;
    // a + b; a / b; a * b; a - b; 
    // if/else for/next, etc.
    Expression(Expression), 
}

impl Parse for Statement {
    fn parse(
        parser: &mut crate::parser::Parser, 
        _precedence: Option<crate::parser::Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        println!("{:?}", parser.current_token);
        let stmt = match &parser.current_token {
            Some((_, Token::Var, _)) => Self::Declaration(Declaration::parse(parser, None)?),
            // Some((start, Token::Begin, end)) if parser.is_parsing_program => Self::Block(Block::parse(parser, None)?),
            Some((_, Token::Begin, _)) if !parser.is_parsing_program => Self::Program(Program::parse(parser, None)?),
            Some((_, tok, _)) => Self::Expression(Expression::parse(parser, None)?),
            None => return parse_error(
                ParseErrorType::UnexpectedEof, 
                SrcSpan { start: 0, end: 0 }
            )
        };

        match &parser.next_token {
            Some((_, tok, _)) if tok.is_ending() => {
                parser.step();
            },
            _ => {}
        }

        Ok(stmt)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Parse for Program {
    fn parse(
        parser: &mut crate::parser::Parser, 
        _precedence: Option<crate::parser::Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let _ = parser.expect_one(Token::Begin).unwrap();
        parser.is_parsing_program = true;

        let mut statements = vec![];

        while parser.current_token.as_ref()
            .is_some_and(|(_, token, _)| *token != Token::End) 
        {
            statements.push(Statement::parse(parser, None)?);

            match (&parser.current_token, &parser.next_token) {
                (Some((start, Token::Semicolon, end)), Some((_, Token::End, _))) => {
                    return parse_error(
                        ParseErrorType::UnexpectedToken { 
                            token: Token::Semicolon, 
                            expected: Token::End 
                        }, 
                        SrcSpan { start: *start, end: *end }
                    );
                },
                (Some((_, Token::End, _)), None) => {
                    parser.step();
                    parser.is_parsing_program = false;

                    break;
                },
                _ => {
                    parser.step();
                }
            }
        }

        Ok(Self {
            statements
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IdentifierType {
    String, // @
    Float, // !
    Int, // %
    Bool // $
}

impl From<Token> for IdentifierType {
    fn from(value: Token) -> Self {
        match value {
            Token::At => Self::String,
            Token::Bang => Self::Float,
            Token::Percent => Self::Int,
            Token::Dollar => Self::Bool,
            _ => panic!("Invalid token to identifier type conversion")
        }
    }
}

impl Display for IdentifierType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let type_str = match self {
            Self::String => "@",
            Self::Float => "!",
            Self::Int => "%",
            Self::Bool => "$",
        };

        write!(f, "{type_str}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Declaration {
    pub name: Identifier,
    pub ident_type: IdentifierType,
}

impl Parse for Declaration {
    fn parse(
        parser: &mut crate::parser::Parser, 
        _precedence: Option<crate::parser::Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let _ = parser.expect_one(Token::Var).unwrap();

        let (_, ident, _) = parser.expect_ident()?;

        let (_, ident_type, _) = parser.parse_type_annotation(Token::Colon)?;

        Ok(Self {
            name: Identifier { value: ident },
            ident_type
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub expressions: Vec<Expression>
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = self.expressions.iter()
            .map(|expr| expr.to_string())
            .collect::<Vec<String>>().join("\n\t");

        write!(f, "begin {string} end")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Primitive(Primitive),
    Prefix(Prefix),
    Infix(Infix),
    Conditional(Conditional),
    FixedLoop(FixedLoop),
    ConditionalLoop(ConditionalLoop),
    // FunctionCall(FunctionCall),
}

impl Parse for Expression {
    fn parse(
        parser: &mut crate::parser::Parser, 
        precedence: Option<crate::parser::Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let expr = match &parser.current_token {
            Some((_, token, _)) => match token {
                Token::Int(_) | Token::Float(_) | Token::Hexadecimal(_) | Token::Octal(_) | Token::Binary(_) | Token::True | Token::False | Token::String(_) => {
                    Self::Primitive(Primitive::parse(parser, None)?)
                },
                _ => todo!("no such expression parser"),
            },
            _ => todo!("unexpected eof")
        };

        Ok(expr)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub value: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    Int(i64),
    Float(f64),
    Hex(i64),
    Octal(i64),
    Bin(i64),
    String(String),
    Bool(bool)
}

impl Parse for Primitive {
    fn parse(
        parser: &mut crate::parser::Parser, 
        _precedence: Option<crate::parser::Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let primitive = match &parser.current_token {
            Some((_, token, _)) => match token {
                Token::Int(value) => {
                    let parsed = value.parse();

                    match parsed {
                        Ok(value) => Self::Int(value),
                        Err(err) => {
                            todo!("parse error");
                        }
                    }
                },
                Token::Float(value) => {
                    let parsed = value.parse();

                    match parsed {
                        Ok(value) => Self::Float(value),
                        Err(err) => {
                            todo!("parse error");
                        }
                    }
                },
                Token::Hexadecimal(value) => {
                    let parsed = i64::from_str_radix(&value, 16);

                    match parsed {
                        Ok(value) => Self::Hex(value),
                        Err(_) => {
                            todo!("parse error");
                        }
                    }
                },
                Token::Octal(value) => {
                    let parsed = i64::from_str_radix(&value, 8);

                    match parsed {
                        Ok(value) => Self::Octal(value),
                        Err(_) => {
                            todo!("parse error");
                        }
                    }
                },
                Token::Binary(value) => {
                    let parsed = i64::from_str_radix(&value, 2);

                    match parsed {
                        Ok(value) => Self::Bin(value),
                        Err(_) => {
                            todo!("parse error");
                        }
                    }
                },
                Token::String(value) => {
                    Self::String(value.to_owned())
                },
                Token::True => Self::Bool(true),
                Token::False => Self::Bool(false),
                token => {
                    todo!("unexpected token");
                }
            },
            None => {
                todo!("unexpected eof");
            }
        };

        Ok(primitive)
    }
}

impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(value) => write!(f, "{}", value),
            Self::Float(value) => write!(f, "{}", value),
            Self::Hex(value) => write!(f, "{:x}", value),
            Self::Octal(value) => write!(f, "{:o}", value),
            Self::Bin(value) => write!(f, "{:b}", value),
            Self::String(value) => write!(f, "\"{}\"", value),
            Self::Bool(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Prefix {
    pub operator: Token,
    pub right: Box<Expression>
}

#[derive(Debug, Clone, PartialEq)]
pub struct Infix {
    pub left: Box<Expression>,
    pub operator: Token,
    pub right: Box<Expression>,
}

impl InfixParse for Infix {
    fn parse(
        parser: &mut crate::parser::Parser, 
        left: Expression,
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let precedence = parser.current_precedence();

        let operator = match parser.next_token() {
            Some((start, token, end)) => match token {
                token if token.is_operator() => {
                    token
                },
                _ => return parse_error(
                    ParseErrorType::ExpectedOperator,
                    SrcSpan { start, end }
                )
            },
            None => return parse_error(
                ParseErrorType::UnexpectedEof, 
                SrcSpan { start: 0, end: 0 }
            )
        };

        let right = Expression::parse(parser, Some(precedence))?;

        Ok(Infix::from(left, operator, right))
    }
}

impl Display for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator.as_literal(), self.right)
    }
}

impl Infix {
    pub fn from(left: Expression, operator: Token, right: Expression) -> Self {
        Infix {
            left: Box::new(left),
            operator,
            right: Box::new(right)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Conditional {
    pub condition: Infix,
    pub resolution: Box<Expression>,
    pub alternative: Option<Box<Expression>>,
}

impl Parse for Conditional {
    fn parse(
        parser: &mut crate::parser::Parser, 
        precedence: Option<crate::parser::Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let _ = parser.expect_one(Token::If).unwrap();

        let condition = Expression::parse(parser, Some(Precedence::Lowest))?;

        todo!();
    }
}

impl Display for Conditional {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FixedLoop {

}

#[derive(Debug, Clone, PartialEq)]
pub struct ConditionalLoop {

}