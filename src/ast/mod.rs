mod primitive;
pub use primitive::Primitive;

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
        // println!("{:?}, {:?}", parser.current_token, parser.next_token);
        let stmt = match &parser.current_token {
            Some((_, Token::Var, _)) => Self::Declaration(Declaration::parse(parser, None)?),
            Some((_, Token::Begin, _)) if parser.is_parsing_program => Self::Block(Block::parse(parser, None)?),
            Some((_, Token::Begin, _)) if !parser.is_parsing_program => Self::Program(Program::parse(parser, None)?),
            Some(_) => Self::Expression(Expression::parse(parser, None)?),
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

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Program(program) => write!(f, "{}", program),
            Self::Declaration(decl) => write!(f, "{}", decl),
            Self::Block(block) => write!(f, "{}", block),
            Self::Expression(expr) => write!(f, "{}", expr)
        }
    }
}

impl Statement {
    pub fn location(&self) -> SrcSpan {
        match self {
            Self::Program(program) => program.location,
            Self::Declaration(decl) => decl.location,
            Self::Block(block) => block.location,
            Self::Expression(expr) => expr.location()
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub location: SrcSpan,
}

impl Parse for Program {
    fn parse(
        parser: &mut crate::parser::Parser, 
        _precedence: Option<crate::parser::Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let (start, _, _) = parser.next_token().unwrap();
        let mut end = 0;
        parser.is_parsing_program = true;

        let mut statements = vec![];

        while parser.current_token.as_ref()
            .is_some_and(|(_, token, _)| *token != Token::End) 
        {
            statements.push(Statement::parse(parser, None)?);

            println!("prog: {:?}, {:?}", parser.current_token, parser.next_token);

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
                (Some((_, Token::Semicolon, _)), Some(_)) => {
                    println!("after semi");
                    parser.step();
                }
                (Some((_, Token::End, _)), None) => {
                    end = parser.next_token().unwrap().2;
                    parser.is_parsing_program = false;

                    break;
                },
                // probably missing semicolon
                (Some((start, tok, end)), _) => return parse_error(
                    ParseErrorType::UnexpectedToken { 
                        token: tok.clone(), 
                        expected: Token::Semicolon 
                    },
                    SrcSpan { start: *start, end: *end }
                ),
                (None, None) => return parse_error(
                    ParseErrorType::UnexpectedEof,
                    SrcSpan { start: 0, end: 0 }
                ),
                _ => unreachable!("Program parser should not reach this place")
            }
        }

        Ok(Self {
            statements,
            location: SrcSpan { start, end }
        })
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = self.statements.iter()
            .map(|expr| expr.to_string())
            .collect::<Vec<String>>().join(";\n\t");

        write!(f, "begin\n\t{string}\nend")
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
    pub names: Vec<Identifier>,
    pub ident_type: IdentifierType,
    pub location: SrcSpan,
}

impl Parse for Declaration {
    fn parse(
        parser: &mut crate::parser::Parser, 
        _precedence: Option<crate::parser::Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let (start, _) = parser.expect_one(Token::Var).unwrap();

        let mut names = vec![];

        names.push(parser.expect_ident()?);
        // println!("{names:?}");
        // println!("{:?}", parser.current_token);

        while let Ok(_) = parser.expect_one(Token::Comma) {
            // println!("got comma");
            names.push(parser.expect_ident()?);
        }

        // println!("{names:?}");
        // println!("{:?}", parser.current_token);

        let (_, ident_type, end) = parser.parse_type_annotation(Token::Colon)?;
        // println!("{:?}", parser.current_token);

        let idents = names.into_iter()
            .map(|ident| Identifier { 
                value: ident.1, 
                location: SrcSpan { 
                    start: ident.0, 
                    end: ident.2 
                } 
            })
            .collect::<Vec<Identifier>>();

        Ok(Self {
            names: idents,
            ident_type,
            location: SrcSpan {
                start,
                end
            }
        })
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let names = self.names.iter()
            .map(|ident| ident.value.clone())
            .collect::<Vec<String>>();

        write!(f, "var {}: {}", names.join(", "), self.ident_type)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub expressions: Vec<Expression>,
    pub location: SrcSpan
}

impl Parse for Block {
    fn parse(
        parser: &mut crate::parser::Parser, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let (start, _) = parser.expect_one(Token::Begin)?;

        let mut expressions = vec![];

        while parser.current_token.as_ref()
            .is_some_and(|(_, token, _)| *token != Token::End) 
        {
            expressions.push(Expression::parse(parser, None)?);

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
                (Some((_, Token::End, _)), _) => {
                    break;
                },
                _ => {
                    parser.step();
                }
            }
        }

        let (_, end)  = parser.expect_one(Token::End)?;

        Ok(Self {
            expressions,
            location: SrcSpan { start, end }
        })
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = self.expressions.iter()
            .map(|expr| expr.to_string())
            .collect::<Vec<String>>().join(";\n\t");

        write!(f, "begin\n\t{string}\nend")
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
        let mut expr = match parser.current_token.clone() {
            Some((start, token, end)) => match token {
                Token::Ident(ident) => {
                    parser.step();

                    Self::Identifier(Identifier { 
                        value: ident,
                        location: SrcSpan { start, end } 
                    })
                },
                Token::Int(_) | Token::Float(_) | 
                Token::Hexadecimal(_) | Token::Octal(_) | 
                Token::Binary(_) | Token::True | 
                Token::False | Token::String(_) => {
                    Self::Primitive(Primitive::parse(parser, None)?)
                },
                Token::Bang | Token::Minus => {
                    Self::Prefix(Prefix::parse(parser, None)?)
                },
                Token::LParen => {
                    parser.step();

                    let expr = Expression::parse(parser, Some(Precedence::Lowest))?;

                    let _ = parser.expect_one(Token::RParen)?;

                    expr
                },
                Token::If => {
                    Self::Conditional(Conditional::parse(parser, None)?)
                },
                Token::While => {
                    Self::ConditionalLoop(ConditionalLoop::parse(parser, None)?)
                },
                Token::For => {
                    Self::FixedLoop(FixedLoop::parse(parser, None)?)
                },
                _ => todo!("no such expression parser"),
            },
            _ => return parse_error(
                ParseErrorType::UnexpectedEof, 
                SrcSpan { start: 0, end: 0 }
            )
        };

        // println!("parse {expr:?}, cur: {:?}", parser.current_token);

        while parser.current_token.as_ref()
            .is_some_and(|token| token.1 != Token::Semicolon) && 
            precedence.unwrap_or(Precedence::Lowest) < parser.current_precedence() 
        {
            expr = match &parser.current_token {
                Some((_, next_token, _)) => match next_token {
                    Token::Plus | Token::Minus | Token::Slash | 
                    Token::Asterisk | Token::Equal | Token::NotEqual | 
                    Token::LessThan | Token::GreaterThan | Token::LessThanOrEqual | 
                    Token::GreaterThanOrEqual | Token::Assign => {
                        Self::Infix(Infix::parse(parser, expr, precedence)?)
                    },
                    _ => break
                },
                None => break
            }
        }

        // println!("parse2 {expr:?}, cur: {:?}", parser.current_token);

        Ok(expr)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(ident) => write!(f, "{}", ident),
            Self::Primitive(primitive) => write!(f, "{}", primitive),
            Self::Prefix(prefix) => write!(f, "{}", prefix),
            Self::Infix(infix) => write!(f, "{}", infix),
            Self::Conditional(conditional) => write!(f, "{}", conditional),
            Self::FixedLoop(loop_) => write!(f, "{}", loop_),
            Self::ConditionalLoop(loop_) => write!(f, "{}", loop_)
        }
    }
}

impl Expression {
    pub fn location(&self) -> SrcSpan {
        match self {
            Self::Identifier(ident) => ident.location,
            Self::Primitive(primitive) => primitive.location(),
            Self::Prefix(prefix) => prefix.location,
            Self::Infix(infix) => infix.location,
            Self::Conditional(conditional) => conditional.location,
            Self::FixedLoop(loop_) => loop_.location,
            Self::ConditionalLoop(loop_) => loop_.location
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockExpression {
    Block(Block),
    Expression(Expression),
}

impl Parse for BlockExpression {
    fn parse(
        parser: &mut crate::parser::Parser, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        // println!("blockExpression parse");
        // println!("cur: {:?}, nxt: {:?}", parser.current_token, parser.next_token);
        let res = match &parser.current_token {
            Some((_, Token::Begin, _)) => Self::Block(Block::parse(parser, None)?),
            Some(_) => Self::Expression(Expression::parse(parser, None)?),
            None => return parse_error(
                ParseErrorType::UnexpectedEof, 
                SrcSpan { start: 0, end: 0 }
            )
        };
        
        // println!("blockExpression got: {res:?}");

        Ok(res)
    }
}

impl Display for BlockExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Block(block) => write!(f, "{}", block),
            Self::Expression(expr) => write!(f, "{}", expr)
        }
    }
}

impl BlockExpression {
    pub fn location(&self) -> SrcSpan {
        match self {
            Self::Block(block) => block.location,
            Self::Expression(expr) => expr.location()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub value: String,
    pub location: SrcSpan
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Prefix {
    pub operator: Token,
    pub right: Box<Expression>,
    pub location: SrcSpan
}

impl Parse for Prefix {
    fn parse(
        parser: &mut crate::parser::Parser, 
        precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        todo!();
    }
}

impl Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

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

impl Infix {
    pub fn from(
        left: Expression, 
        operator: Token, 
        right: Expression,
        location: SrcSpan
    ) -> Self {
        Infix {
            left: Box::new(left),
            operator,
            right: Box::new(right),
            location
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Conditional {
    pub condition: Infix,
    pub resolution: Box<BlockExpression>,
    pub alternative: Option<Box<BlockExpression>>,
    pub location: SrcSpan
}

impl Parse for Conditional {
    fn parse(
        parser: &mut crate::parser::Parser, 
        precedence: Option<crate::parser::Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        // println!("parsing conditional");
        let (start, _) = parser.expect_one(Token::If).unwrap();

        let condition = match Expression::parse(parser, Some(Precedence::Lowest)) {
            Ok(Expression::Infix(infix)) => infix,
            Ok(_) => return parse_error(
                ParseErrorType::ExpectedInfix, 
                SrcSpan { start: 0, end: 0 }
            ),
            Err(err) => return Err(err)
        };

        // println!("got condition: {condition:?}");

        let resolution = Box::new(
            BlockExpression::parse(parser, None)?
        );

        // println!("got resolution: {resolution:?}");

        let mut end = resolution.location().end;

        let alternative = match parser.expect_one(Token::Else) {
            Ok(_) => {
                let block_expr = BlockExpression::parse(parser, None)?;

                end = block_expr.location().end;

                Some(Box::new(block_expr))
            },
            Err(_) => None
        };

        // println!("got alternative: {alternative:?}");
        // println!("| {:?}, {:?}", parser.current_token, parser.next_token);

        Ok(Self {
            condition,
            resolution,
            alternative,
            location: SrcSpan { start, end }
        })
    }
}

impl Display for Conditional {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let alternative = match &self.alternative {
            Some(alternative) => format!(" else {}", alternative),
            None => "".to_string()
        };

        write!(f, "if ({}) {}{}", self.condition, self.resolution, alternative)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FixedLoop { // for
    pub value: Infix, // i := 5
    pub to: Primitive, // to 10
    pub step: Option<Primitive>, // step 1
    pub block: Box<BlockExpression>,
    pub location: SrcSpan
}

impl Parse for FixedLoop {
    fn parse(
        parser: &mut crate::parser::Parser, 
        _precedence: Option<crate::parser::Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let (start, _) = parser.expect_one(Token::For).unwrap();

        let value = match Expression::parse(parser, Some(Precedence::Lowest)) {
            Ok(Expression::Infix(infix)) => infix,
            Ok(_) => return parse_error(
                ParseErrorType::ExpectedInfix, 
                SrcSpan { start: 0, end: 0 }
            ),
            Err(err) => return Err(err)
        };

        let _ = parser.expect_one(Token::To)?;

        let to = Primitive::parse(parser, None)?;

        let step = match parser.expect_one(Token::Step) {
            Ok(_) => Some(
                Primitive::parse(parser, None)?
            ),
            Err(_) => None
        };

        let block = BlockExpression::parse(parser, None)?;

        let (_, end) = parser.expect_one(Token::Next)?;

        Ok(Self {
            value,
            to,
            step,
            block: Box::new(block),
            location: SrcSpan { start, end }
        })
    }
}

impl Display for FixedLoop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let step = match &self.step {
            Some(step) => format!(" step {step}"),
            None => "".to_string()
        };
        
        write!(f, "for {} to {}{} {} next", self.value, self.to, step, self.block)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConditionalLoop { // while
    pub condition: Infix, // (a < 10)
    pub block: Box<BlockExpression>, // a := a + 1
    pub location: SrcSpan
}

impl Parse for ConditionalLoop {
    fn parse(
        parser: &mut crate::parser::Parser, 
        _precedence: Option<crate::parser::Precedence>
    ) -> Result<Self, crate::parser::ParseError> {
        let (start, _) = parser.expect_one(Token::While).unwrap();

        let condition = match Expression::parse(parser, Some(Precedence::Lowest)) {
            Ok(Expression::Infix(infix)) => infix,
            Ok(_) => return parse_error(
                ParseErrorType::ExpectedInfix, 
                SrcSpan { start: 0, end: 0 }
            ),
            Err(err) => return Err(err)
        };
        // println!("got condition: {condition:?}");

        let block = BlockExpression::parse(parser, None)?;
        // println!("got block: {block:?}");

        let end = block.location().end;
        
        Ok(Self {
            condition,
            block: Box::new(block),
            location: SrcSpan { start, end }
        })
    }
}

impl Display for ConditionalLoop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "while ({}) {}", self.condition, self.block)
    }
}