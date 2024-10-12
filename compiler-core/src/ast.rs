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
    pub program: Program
}

// program -> begin <statement> {; <statement> } end
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub location: SrcSpan
}

impl Parse for Program {
    fn parse(
        parser: &mut crate::parser::Parser, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let (start, mut end) = parser.expect_one(Token::Begin)?;

        let mut statements = vec![];

        while let Some((start, token, _end)) = parser.current_token.take() {
            if token != Token::End {
                parser.current_token = Some((start, token, _end));

                // println!("prog1 | {:?}, {:?}", parser.current_token, parser.next_token);
                
                statements.push(Statement::parse(parser, None)?);

                // println!("prog2 | {:?}, {:?}", parser.current_token, parser.next_token);

                match (&parser.current_token, &parser.next_token) {
                    (
                        Some((start, Token::Semicolon, end)), 
                        Some((_, Token::End, _))
                    ) => return parse_error(
                        ParseErrorType::UnexpectedSemicolonBeforeEnd, 
                        SrcSpan { start: *start, end: *end }
                    ),
                    (Some((_, Token::Semicolon, _)), _) => parser.step(),
                    (Some((_, Token::End, _)), _) => {
                        end = parser.next_token().unwrap().2;
                        break 
                    },
                    (None, _) => return parse_error(
                        ParseErrorType::UnexpectedEof, 
                        SrcSpan { start: 0, end: 0 }
                    ),
                    (Some((start, _, _)), _) => return parse_error(
                        ParseErrorType::MissingSemicolon,
                        SrcSpan { start: *start - 1, end: *start }
                    ),
                }
            } else {
                end = _end;
                parser.step();
                break;
            }
        };

        Ok(Self {
            statements,
            location: SrcSpan { start, end }
        })
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let statements = self.statements.iter()
            .map(|statement| format!("{}", statement))
            .collect::<Vec<String>>();

        write!(f, "begin {} end", statements.join("; "))
    }
}

// statement -> (<declaration> | <operator>)
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Declaration(Declaration),
    Operator(Operator),
}

impl Parse for Statement {
    fn parse(
        parser: &mut crate::parser::Parser, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let res = match parser.current_token {
            Some((_, Token::Var, _)) => Self::Declaration(Declaration::parse(parser, None)?),
            Some(_) => Self::Operator(Operator::parse(parser, None)?),
            None => return parse_error(
                ParseErrorType::UnexpectedEof, 
                SrcSpan { start: 0, end: 0 }
            )
        };

        Ok(res)  
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Declaration(declaration) => write!(f, "{declaration}"),
            Self::Operator(operation) => write!(f, "{operation}")
        }
    }
}

// identifiers -> <identifier> {, <identifier> } : <type>
#[derive(Debug, Clone, PartialEq)]
pub struct Identifiers {
    pub names: Vec<Identifier>,
    pub names_type: IdentifierType
}

impl Display for Identifiers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let names = self.names.iter()
            .map(|name| format!("{name}"))
            .collect::<Vec<String>>();

        write!(f, "{}: {}", names.join(", "), self.names_type)
    }
}

// type -> @ | ! | % | $
#[derive(Debug, Clone, PartialEq)]
pub enum IdentifierType {
    String, // @
    Float, // !
    Int, // %
    Bool // $
}

impl Display for IdentifierType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ident_type = match self {
            Self::String => "@",
            Self::Float => "!",
            Self::Int => "%",
            Self::Bool => "$"
        };

        write!(f, "{ident_type}")
    }
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

// declaration -> var { <identifiers> ;}
#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
    pub identifiers: Vec<Identifiers>,
    pub location: SrcSpan
}

impl Parse for Declaration {
    fn parse(
        parser: &mut crate::parser::Parser, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let (start, mut end) = parser.expect_one(Token::Var)?;

        let mut identifiers_vec = vec![];

        while let Ok(ident) = parser.expect_ident() {
            let mut names = vec![Identifier::from(ident)];

            while let Ok(_) = parser.expect_one(Token::Comma) {
                names.push(parser.expect_ident()?.into());
            }

            let (_, names_type, _) = parser.parse_type_annotation(Token::Colon)?;

            end = parser.expect_one(Token::Semicolon)?.1;

            identifiers_vec.push(Identifiers {
                names,
                names_type
            });
        }

        Ok(Declaration {
            identifiers: identifiers_vec,
            location: SrcSpan { start, end }
        })
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let identifiers = self.identifiers.iter()
            .map(|idents| idents.to_string())
            .collect::<Vec<String>>();

        if identifiers.len() > 0 {
            write!(f, "var {};", identifiers.join("; "))
        } else {
            write!(f, "var")
        }
    }
}

// operator -> <nested> | <assignment> | <conditional> | <fixed_loop> | <conditional_loop> | <input> | <output>
#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Nested(Nested),
    Assignment(Assignment),
    Conditional(Conditional),
    FixedLoop(FixedLoop),
    ConditionalLoop(ConditionalLoop),
    Input(Input),
    Output(Output),
}

impl Parse for Operator {
    fn parse(
        parser: &mut crate::parser::Parser, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let res = match &parser.current_token {
            Some((start, token, end)) => match token {
                Token::Begin => Self::Nested(Nested::parse(parser, None)?),
                Token::Ident(_) => Self::Assignment(Assignment::parse(parser, None)?),
                Token::If => Self::Conditional(Conditional::parse(parser, None)?),
                Token::For => Self::FixedLoop(FixedLoop::parse(parser, None)?),
                Token::While => Self::ConditionalLoop(ConditionalLoop::parse(parser, None)?),
                Token::Readln => Self::Input(Input::parse(parser, None)?),
                Token::Writeln => Self::Output(Output::parse(parser, None)?),
                _ => return parse_error(
                    ParseErrorType::UnexpectedToken { 
                        token: token.clone(), 
                        expected: vec!["Any operator".to_string()] 
                    },
                    SrcSpan { start: *start, end: *end }
                )
            },
            None => return parse_error(
                ParseErrorType::UnexpectedEof, 
                SrcSpan { start: 0, end: 0 }
            )
        };

        Ok(res)
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nested(nested) => write!(f, "{nested}"),
            Self::Assignment(assignment) => write!(f, "{assignment}"),
            Self::Conditional(conditional) => write!(f, "{conditional}"),
            Self::FixedLoop(loop_) => write!(f, "{loop_}"),
            Self::ConditionalLoop(loop_) => write!(f, "{loop_}"),
            Self::Input(input) => write!(f, "{input}"),
            Self::Output(output) => write!(f, "{output}")
        }
    }
}

impl Operator {
    pub fn location(&self) -> SrcSpan {
        match self {
            Self::Nested(nested) => nested.location,
            Self::Assignment(assignment) => assignment.location,
            Self::Conditional(conditional) => conditional.location,
            Self::FixedLoop(loop_) => loop_.location,
            Self::ConditionalLoop(loop_) => loop_.location,
            Self::Input(input) => input.location,
            Self::Output(output) => output.location
        }
    }
}

// nested -> begin <operator> {; <operator> } end
#[derive(Debug, Clone, PartialEq)]
pub struct Nested {
    pub operators: Vec<Operator>,
    pub location: SrcSpan
}

impl Parse for Nested {
    fn parse(
        parser: &mut crate::parser::Parser, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let (start, mut end) = parser.expect_one(Token::Begin)?;

        let mut operators = vec![];

        while let Some((start, token, _end)) = parser.current_token.take() {
            if token != Token::End {
                parser.current_token = Some((start, token, _end));

                // println!("nest1 | {:?}, {:?}", parser.current_token, parser.next_token);

                operators.push(Operator::parse(parser, None)?);

                // println!("nest2 | {:?}, {:?}", parser.current_token, parser.next_token);

                match (&parser.current_token, &parser.next_token) {
                    (
                        Some((start, Token::Semicolon, end)), 
                        Some((_, Token::End, _))
                    ) => return parse_error(
                        ParseErrorType::UnexpectedSemicolonBeforeEnd, 
                        SrcSpan { start: *start, end: *end }
                    ),
                    (Some((_, Token::Semicolon, _)), _) => parser.step(),
                    (Some((_, Token::End, _)), _) => {
                        end = parser.next_token().unwrap().2;
                        break 
                    },
                    (None, _) => return parse_error(
                        ParseErrorType::UnexpectedEof, 
                        SrcSpan { start: 0, end: 0 }
                    ),
                    (Some((start, _, _)), _) => return parse_error(
                        ParseErrorType::MissingSemicolon,
                        SrcSpan { start: *start - 1, end: *start }
                    ),
                }
            } else {
                end = _end;
                parser.step();
                break;
            }
        };

        Ok(Self {
            operators,
            location: SrcSpan { start, end }
        })
    }
}

impl Display for Nested {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let operators = self.operators.iter()
            .map(|operator| format!("{}", operator))
            .collect::<Vec<String>>();

        write!(f, "begin {} end", operators.join("; "))
    }
}

// assignment -> <identifier> := <expression>
#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub identifier: Identifier,
    pub value: Expression,
    pub location: SrcSpan
}

impl Parse for Assignment {
    fn parse(
        parser: &mut crate::parser::Parser,
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        // println!("assign");
        let ident = parser.expect_ident()?;
        let start = ident.0;

        let (_, end) = parser.expect_one(Token::Assign)?;

        let value = match Expression::parse(parser, None) {
            Ok(value) => value,
            Err(_) => return parse_error(
                ParseErrorType::ExpectedValue,
                SrcSpan { start: end, end }
            )
        };
        let end = value.location().end;

        Ok(Self {
            identifier: ident.into(),
            value,
            location: SrcSpan { start, end }
        })
    }
}

impl Display for Assignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} := {}", self.identifier, self.value)
    }
}

// conditional -> if "("<expression>")" <operator> [else <operator>]
#[derive(Debug, Clone, PartialEq)]
pub struct Conditional {
    pub condition: Expression,
    pub resolution: Box<Operator>,
    pub alternative: Option<Box<Operator>>,
    pub location: SrcSpan
}

impl Parse for Conditional {
    fn parse(
        parser: &mut crate::parser::Parser, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let (start, _) = parser.expect_one(Token::If)?;
        let _ = parser.expect_one(Token::LParen)?;

        let condition = Expression::parse(parser, None)?;
        
        let _ = parser.expect_one(Token::RParen)?;

        let resolution = Box::new(Operator::parse(parser, None)?);

        let mut end = resolution.location().end;

        let alternative = match parser.expect_one(Token::Else) {
            Ok((_, _)) => {
                let alternative = Operator::parse(parser, None)?;

                end = alternative.location().end;

                Some(Box::new(alternative))
            },
            Err(_) => None
        };

        let location = SrcSpan { start, end };

        Ok(Self {
            condition,
            resolution,
            alternative,
            location
        })
    }
}

impl Display for Conditional {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if ({}) {}{}", 
            self.condition, 
            self.resolution, 
            if self.alternative.is_some() {
                format!(" else {}", self.alternative.as_ref().unwrap())
            } else {
                "".to_string()
            }
        )
    }
}

// fixed_loop -> for <assignment> to <expression> [step <expression>] <operator> next
#[derive(Debug, Clone, PartialEq)]
pub struct FixedLoop {
    pub assignment: Assignment,
    pub to: Expression,
    pub step: Option<Expression>,
    pub block: Box<Operator>,
    pub location: SrcSpan
}

impl Parse for FixedLoop {
    fn parse(
        parser: &mut crate::parser::Parser, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let (start, _) = parser.expect_one(Token::For)?;

        let assignment = Assignment::parse(parser, None)?;
        let _ = parser.expect_one(Token::To);

        let to = Expression::parse(parser, None)?;

        let step = match parser.expect_one(Token::Step) {
            Ok(_) => Some(Expression::parse(parser, None)?),
            Err(_) => None
        };

        let block = Box::new(Operator::parse(parser, None)?);
        let (_, end) = parser.expect_one(Token::Next)?;

        let location = SrcSpan { start, end };

        Ok(Self {
            assignment,
            to,
            step,
            block,
            location
        })
    }
}

impl Display for FixedLoop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "for {} to {}{} {} next",
            self.assignment,
            self.to,
            if self.step.is_some() {
                format!(" step {}", self.step.as_ref().unwrap())
            } else {
                "".to_string()
            },
            self.block
        )
    }
}

// conditional_loop -> while "(" <expression> ")" <operator>
#[derive(Debug, Clone, PartialEq)]
pub struct ConditionalLoop {
    pub condition: Expression,
    pub block: Box<Operator>,
    pub location: SrcSpan
}

impl Parse for ConditionalLoop {
    fn parse(
        parser: &mut crate::parser::Parser, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let (start, _) = parser.expect_one(Token::While)?;

        let _ = parser.expect_one(Token::LParen)?;

        let condition = Expression::parse(parser, None)?;
        let _ = parser.expect_one(Token::RParen)?;

        let block = Box::new(Operator::parse(parser, None)?);
        let end = block.location().end;

        let location = SrcSpan { start, end };

        Ok(Self {
            condition,
            block,
            location
        })
    }
}

impl Display for ConditionalLoop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "while ({}) {}", self.condition, self.block)
    }
}

// input -> readln <identifier> {, <identifier> }
#[derive(Debug, Clone, PartialEq)]
pub struct Input {
    pub identifiers: Vec<Identifier>,
    pub location: SrcSpan
}

impl Parse for Input {
    fn parse(
        parser: &mut crate::parser::Parser, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let (start, _) = parser.expect_one(Token::Readln)?;

        let mut identifiers = vec![Identifier::from(parser.expect_ident()?)];

        while let Ok(_) = parser.expect_one(Token::Comma) {
            identifiers.push(parser.expect_ident()?.into());
        }

        let end = identifiers.iter().last().unwrap().location.end;

        let location = SrcSpan { start, end };

        Ok(Self {
            identifiers,
            location
        })
    }
}

impl Display for Input {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let identifiers = self.identifiers.iter()
            .map(|ident| ident.value.clone())
            .collect::<Vec<String>>();

        write!(f, "readln {}", identifiers.join(", "))
    }
}

// output -> writeln <expression> {, <expression> }
#[derive(Debug, Clone, PartialEq)]
pub struct Output {
    pub expressions: Vec<Expression>,
    pub location: SrcSpan
}

impl Parse for Output {
    fn parse(
        parser: &mut crate::parser::Parser, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let (start, _) = parser.expect_one(Token::Writeln)?;

        let mut expressions = vec![Expression::parse(parser, None)?];

        while let Ok(_) = parser.expect_one(Token::Comma) {
            expressions.push(Expression::parse(parser, None)?);
        }

        let end = expressions.iter().last().unwrap().location().end;

        let location = SrcSpan { start, end };

        Ok(Self {
            expressions,
            location
        })    
    }
}

impl Display for Output {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let expressions = self.expressions.iter()
            .map(|expr| expr.to_string())
            .collect::<Vec<String>>();

        write!(f, "writeln {}", expressions.join(", "))
    }
}

// expression -> <identifier> | <infix> | <prefix> | <primitive> | "(" <expression> ")" 
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Infix(Infix),
    Prefix(Prefix),
    Primitive(Primitive),
    Nested {
        expression: Box<Expression>,
        location: SrcSpan
    }
}

impl Parse for Expression {
    fn parse(
        parser: &mut crate::parser::Parser, 
        precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        // println!("{:?}, {:?}", parser.current_token, parser.next_token);
        let mut expr = match &parser.current_token {
            Some((start, token, end)) => match token {
                Token::Ident(_) => {
                    let (start, ident, end) = parser.expect_ident()?;

                    Self::Identifier(Identifier::from((start, ident, end)))
                },
                Token::Bang => Self::Prefix(Prefix::parse(parser, None)?),
                Token::Int(_) | Token::Float(_) | Token::Binary(_) | 
                Token::Octal(_) | Token::Hexadecimal(_) | 
                Token::String(_) | Token::True | Token::False => 
                    Self::Primitive(Primitive::parse(parser, None)?),
                Token::LParen => {
                    let (start, _) = parser.expect_one(Token::LParen)?;

                    let expression = Box::new(Expression::parse(parser, None)?);

                    let (_, end) = parser.expect_one(Token::RParen)?;

                    Self::Nested {
                        expression,
                        location: SrcSpan { start, end }
                    }
                }
                _ => return parse_error(
                    ParseErrorType::UnexpectedToken {
                        token: token.clone(),
                        expected: vec!["an Identifier, `!`, Number or `(`".to_string()]
                    },
                    SrcSpan { start: *start, end: *end }
                )
            },
            None => return parse_error(
                ParseErrorType::UnexpectedEof, 
                SrcSpan { start: 0, end: 0 }
            )
        };

        while parser.current_token.as_ref()
            .is_some_and(|token| token.1 != Token::Semicolon) && 
            precedence.unwrap_or(Precedence::Lowest) < parser.current_precedence() 
        {
            // println!("hey");
            expr = match &parser.current_token {
                Some((_, next_token, _)) => match next_token {
                    Token::Plus | Token::Minus | Token::Slash | 
                    Token::Asterisk | Token::Equal | Token::NotEqual | 
                    Token::LessThan | Token::GreaterThan | 
                    Token::LessThanOrEqual | Token::And | Token::Or |
                    Token::GreaterThanOrEqual => {
                        Self::Infix(Infix::parse(parser, expr, precedence)?)
                    },
                    _ => break
                },
                None => break
            }
        }

        Ok(expr) 
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(ident) => write!(f, "{ident}"),
            Self::Infix(infix) => write!(f, "{infix}"),
            Self::Prefix(prefix) => write!(f, "{prefix}"),
            Self::Primitive(primitive) => write!(f, "{primitive}"),
            Self::Nested { expression, .. } => write!(f, "({expression})")
        }
    }
}

impl Expression {
    pub fn location(&self) -> SrcSpan {
        match self {
            Self::Identifier(ident) => ident.location,
            Self::Infix(infix) => infix.location,
            Self::Prefix(prefix) => prefix.location,
            Self::Primitive(primitive) => primitive.location(),
            Self::Nested { expression, .. } => expression.location()
        }
    }
}

// identifier -> <letter> | { (<letter> | <number>) }
#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub value: String,
    pub location: SrcSpan
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl From<(u32, String, u32)> for Identifier {
    fn from(value: (u32, String, u32)) -> Self {
        Identifier {
            value: value.1,
            location: SrcSpan { start: value.0, end: value.2 }
        }
    }
}

// infix -> <expression> <operator> <expression>
#[derive(Debug, Clone, PartialEq)]
pub struct Infix {
    pub left: Box<Expression>,
    pub operator: Token,
    pub right: Box<Expression>,
    pub location: SrcSpan
}

impl InfixParse for Infix {
    fn parse(
        parser: &mut crate::parser::Parser, 
        left: Expression, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let precedence = parser.current_precedence();

        let SrcSpan { start, .. } = left.location();

        let operator = match &parser.current_token {
            Some((start, token, end)) => match token {
                token if token.is_operator() => {
                    parser.next_token().unwrap().1
                },
                _ => return parse_error(
                    ParseErrorType::ExpectedOperator,
                    SrcSpan { start: *start, end: *end }
                )
            },
            None => return parse_error(
                ParseErrorType::UnexpectedEof, 
                SrcSpan { start: 0, end: 0 }
            )
        };

        let right = Expression::parse(parser, Some(precedence))?;

        let SrcSpan { end, .. } = right.location();

        Ok(Self {
            left: Box::new(left), 
            operator,
            right: Box::new(right), 
            location: SrcSpan { start, end }
        })
    }
}

impl Display for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.left, self.operator.as_literal(), self.right)
    }
}

// prefix -> <unary_operation> <expression>
#[derive(Debug, Clone, PartialEq)]
pub struct Prefix {
    pub operator: Token,
    pub expression: Box<Expression>,
    pub location: SrcSpan
}

impl Parse for Prefix {
    fn parse(
        parser: &mut crate::parser::Parser, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let (start, token, _) = parser.next_token().unwrap();

        let expression = Expression::parse(parser, Some(Precedence::Prefix))?;
        let end = expression.location().end;

        Ok(Self {
            operator: token, 
            expression: Box::new(expression),
            location: SrcSpan { start, end }
        })
    }
}

impl Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.operator.as_literal(), self.expression)
    }
}

// primitive -> <string> | <float> | <int> | <bool>
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
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let span = parser.next_token();

        match span {
            Some((start, token, end)) => match token {
                Token::Binary(value) => {
                    let value = i64::from_str_radix(&value, 2).unwrap();

                    Ok(Self::Int {
                        value,
                        location: SrcSpan { start, end }
                    })
                },
                Token::Octal(value) => {
                    let value = i64::from_str_radix(&value, 8).unwrap();

                    Ok(Self::Int {
                        value,
                        location: SrcSpan { start, end }
                    })
                },
                Token::Int(value) => {
                    let value = i64::from_str_radix(&value, 10).unwrap();

                    Ok(Self::Int {
                        value,
                        location: SrcSpan { start, end }
                    })
                },
                Token::Hexadecimal(value) => {
                    let value = i64::from_str_radix(&value, 16).unwrap();

                    Ok(Self::Int {
                        value,
                        location: SrcSpan { start, end }
                    })
                },
                Token::Float(value) => {
                    let value = value.parse().unwrap();

                    Ok(Self::Float {
                        value,
                        location: SrcSpan { start, end }
                    })
                },
                Token::String(value) => {
                    Ok(Self::String {
                        value: value.clone(),
                        location: SrcSpan { start, end }
                    })
                },
                Token::True => {
                    Ok(Self::Bool {
                        value: true,
                        location: SrcSpan { start, end }
                    })
                },
                Token::False => {
                    Ok(Self::Bool {
                        value: false,
                        location: SrcSpan { start, end }
                    })
                },
                _ => todo!("parse err Unexpected token, but should be unreachable"),
            },
            None => todo!("parse err Unexpected EOF, but should be unreachable"),
        }    
    }
}

impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int { value, .. } => write!(f, "{value}"),
            Self::Float { value, .. } => write!(f, "{value}"),
            Self::Bool { value, .. } => write!(f, "{value}"),
            Self::String { value, .. } => write!(f, "{value}")
        }
    }
}

impl Primitive {
    pub fn location(&self) -> SrcSpan {
        match self {
            Self::Int { location, .. } |
            Self::Float { location, .. } |
            Self::Bool { location, .. } |
            Self::String { location, .. } => *location
        }
    }
}