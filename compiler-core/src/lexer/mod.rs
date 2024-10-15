mod error;
pub use error::{LexicalError, LexicalErrorType};

#[cfg(test)]
mod tests;

use std::fmt::Display;
use crate::token::Token;

pub type Spanned = (u32, Token, u32);
pub type LexResult = std::result::Result<Spanned, LexicalError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SrcSpan {
    pub start: u32,
    pub end: u32,
}

impl SrcSpan {
	pub fn from(start: u32, end: u32) -> Self {
		Self { start: start as u32, end: end as u32 }
	}
}

impl Display for SrcSpan {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}..{}", self.start, self.end)
	}
}

pub fn str_to_keyword(word: &str) -> Option<Token> {
	Some(match word {
		"begin" => Token::Begin,
		"end" => Token::End,
		"for" => Token::For,
		"to" => Token::To,
		"step" => Token::Step,
		"while" => Token::While,
		"next" => Token::Next,
		"var" => Token::Var,
		"if" => Token::If,
		"else" => Token::Else,
		"true" => Token::True,
		"false" => Token::False,
		"readln" => Token::Readln,
		"writeln" => Token::Writeln,
		_ => return None
	})
}

#[derive(Debug, PartialEq, Eq)]
pub enum NumberType {
	Hex,
	Int,
	Octal,
	Binary,
	Float,
}

#[derive(Debug)]
pub struct Lexer<T: Iterator<Item = (u32, char)>> {
	position: u32,
	next_position: u32,
	ch: Option<char>,
	next_ch: Option<char>,
	input: T,
}

impl<T: Iterator<Item = (u32, char)>> Display for Lexer<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, 
			"Lexer {{\n\tposition: {},\n\tnext_position: {},\n\tch: {:?}, next_ch: {:?}\n}}", 
			self.position, self.next_position, self.ch, self.next_ch
		)
	}
}

impl<T: Iterator<Item = (u32, char)>> Lexer<T> {
    pub fn new(input: T) -> Self {
        let mut lexer = Self {
            position: 0,
            next_position: 0,
            ch: None,
			next_ch: None,
            input,
        };

        lexer.next_char();
        lexer.next_char();

        return lexer;
    }

    pub fn next_token(&mut self) -> LexResult {
		self.skip_whitespace();

		let span  = match self.ch {
			Some(ch) => match ch {
				'(' => {
					if self.next_ch == Some('*') {
						self.lex_comment()
					} else {
						self.eat_one_char(Token::LParen)
					}
				},
				')' => self.eat_one_char(Token::RParen),
				':' => {
					if self.next_ch == Some('=') {
						let start_pos = self.position;
						self.next_char();
						let end_pos = self.position;
						self.next_char();
		
						(start_pos as u32, Token::Assign, end_pos as u32)
					} else {
						self.eat_one_char(Token::Colon)
					}
				},
				';' => self.eat_one_char(Token::Semicolon),
				'!' => {
					if self.next_ch == Some('=') {
						let start_pos = self.position;
						self.next_char();
						let end_pos = self.position;
						self.next_char();
		
						(start_pos as u32, Token::NotEqual, end_pos as u32)
					} else {
						self.eat_one_char(Token::Bang)
					}
				},
				'>' => {
					if self.next_ch == Some('=') {
						let start_pos = self.position;
						self.next_char();
						let end_pos = self.position;
						self.next_char();
		
						(start_pos as u32, Token::GreaterThanOrEqual, end_pos as u32)
					} else {
						self.eat_one_char(Token::GreaterThan)
					}
				},
				'<' => {
					if self.next_ch == Some('=') {
						let start_pos = self.position;
						self.next_char();
						let end_pos = self.position;
						self.next_char();
		
						(start_pos as u32, Token::LessThanOrEqual, end_pos as u32)
					} else {
						self.eat_one_char(Token::LessThan)
					}
				},
				',' => self.eat_one_char(Token::Comma),
				'%' => self.eat_one_char(Token::Percent),
				'@' => self.eat_one_char(Token::At),
				'$' => self.eat_one_char(Token::Dollar),
				'+' => self.eat_one_char(Token::Plus),
				'-' => self.eat_one_char(Token::Minus),
				'*' => self.eat_one_char(Token::Asterisk),
				'/' => self.eat_one_char(Token::Slash),
				'=' if self.next_ch == Some('=') => {
					let start_pos = self.position;
					self.next_char();
					let end_pos = self.position;
					self.next_char();
		
					(start_pos as u32, Token::Equal, end_pos as u32)
				},
				'&' if self.next_ch == Some('&') => {
					let start_pos = self.position;
					self.next_char();
					let end_pos = self.position;
					self.next_char();
		
					(start_pos as u32, Token::And, end_pos as u32)
				},
				'|' if self.next_ch == Some('|') => {
					let start_pos = self.position;
					self.next_char();
					let end_pos = self.position;
					self.next_char();
		
					(start_pos as u32, Token::Or, end_pos as u32)
				},
				'"' => {
					return self.lex_string();
				},
				'a'..='z' | 'A'..='Z' => {
					return Ok(self.lex_ident());
				},
				'0'..='9' | '.' => {
					return self.lex_number();
				},
				c => {
					let location = self.position as u32;
					return Err(LexicalError {
						error: LexicalErrorType::UnrecognizedToken { tok: c },
						location: SrcSpan {
							start: location,
							end: location,
						},
					});
				}
			},
			None => {
				self.eat_one_char(Token::Eof)
			}
		};

		Ok(span)
    }

    fn next_char(&mut self) -> Option<char> {
		let ch = self.ch;

		let next = match self.input.next() {
			Some((pos, ch)) => {
				self.position = self.next_position;
				self.next_position = pos;

				Some(ch)
			},
			None => {
				self.position = self.next_position;
				self.next_position += 1;

				None
			}
		};

		self.ch = self.next_ch;
		self.next_ch = next;

		ch
	}

	fn eat_one_char(&mut self, token: Token) -> Spanned {
		let start_pos = self.position;
		self.next_char();
		let end_pos = self.position;

		(start_pos as u32, token, end_pos as u32)
	}

    fn skip_whitespace(&mut self) {
		while self.ch.is_some_and(|ch| ch.is_ascii_whitespace()) {
			let _ = self.next_char();
		}
	}

	fn lex_ident(&mut self) -> Spanned {
        let start_pos = self.position;
		let mut ident = String::new();

		loop {
			match self.ch {
				Some(ch) if ch.is_ascii_alphanumeric() => ident.push(self.next_char().unwrap()),
				_ => break
			}
		}

        let end_pos = self.position;

        if let Some(tok) = str_to_keyword(&ident) {
           	(start_pos as u32, tok, end_pos as u32)
        } else {
            (start_pos as u32, Token::Ident(ident), end_pos as u32)
        }
	}

	fn lex_string(&mut self) -> LexResult {
		let start = self.position;
		let _ = self.next_char();
		let mut value = String::new();

		loop {
			match self.next_char() {
				Some('"') => break,
				Some(ch) => value.push(ch),
				None => return Err(LexicalError {
					error: LexicalErrorType::UnexpectedStringEnd,
					location: SrcSpan {
						start,
						end: start
					}
				})
			}
		}

		let end = self.position;

		Ok((start, Token::String(value), end))
	} 

	fn lex_number(&mut self) -> LexResult {
		let start_pos = self.position;

		let mut value = String::from("");

		let mut has_period = false;
		let mut has_hex = false;
		let mut has_exponent = false;
		let mut has_number_before_exponent = false;
		let mut has_number_after_exponent = false;

		fn is_ascii_hexalpha(ch: char) -> bool { 
			match ch {
				'a'..='f' | 'A'..='F' => true, 
				_ => false 
			}
		}

		fn is_exponent(ch: char) -> bool {
			match ch {
				'e' | 'E' => true,
				_ => false
			}
		}

		fn is_radix(ch: char) -> bool {
			match ch {
				'B' | 'b' => true,
				'O' | 'o' => true,
				'D' | 'd' => true,
				'H' | 'h' => true,
				_ => false
			}
		}

		loop {
			println!("{:?}", self.ch);
			match self.ch {
				Some(ch) if is_radix(ch) => {
					value.push(self.next_char().unwrap());
				},
				Some(ch) if is_ascii_hexalpha(ch) => {
					if !has_exponent && is_exponent(ch) {
						has_number_before_exponent = value.chars()
							.nth(value.len() - 1)
							.unwrap()
							.is_ascii_digit();

						has_exponent = true;

						value.push(self.next_char().unwrap());

						match self.ch {
							Some('+') | Some('-') => { value.push(self.next_char().unwrap()); },
							_ => {}
						}

						has_number_after_exponent = match self.ch {
							Some(ch) if ch.is_ascii_digit() => true,
							_ => false
						};
					} else {
						if !has_hex {
							has_hex = true;
						}
	
						value.push(self.next_char().unwrap());
					}
				}
				Some(ch) if ch.is_ascii_digit() => {
					value.push(self.next_char().unwrap());
				},
				Some(ch) if ch == '.' => {
					if has_period {
						let _ = self.next_char().unwrap();

						let end_pos = self.position;

						return Err(LexicalError {
							error: LexicalErrorType::MultipleFloatingPoints,
							location: SrcSpan::from(start_pos, end_pos)
						});
					} else {
						has_period = true;
						value.push(self.next_char().unwrap());
					}
				},
				Some(_) => break,
				None => todo!()
			}
		}

		println!("{value} | {:?}", self.ch);
		// todo!();
		// if is_radix(ch) && !self.ch.is_ascii_hexdigit() && self.ch != Some('.') {
		// 	break;
		// } else if !self.ch.is_ascii_hexdigit() && self.ch != Some('.') {
		// 	break;
		// } else if has_period && self.ch == Some('.') {
		// 	self.next_char();

		// 	while self.ch.is_ascii_digit() {
		// 		self.next_char();
		// 	}

		// 	let end_pos = self.position;

		// 	return Err(LexicalError {
		// 		error: LexicalErrorType::MultipleFloatingPoints,
		// 		location: SrcSpan::from(start_pos, end_pos)
		// 	});
		// } else if !has_period && self.ch == '.' {
		// 	has_period = true;
		// } else if !has_hex && is_ascii_hexalpha(self.ch) {
		// 	has_hex = true;
		// }

		// if !has_exponent && is_exponent(self.ch) {
		// 	let has_digit_before_exponent = self.input[self.position - 1]
		// 		.is_ascii_hexdigit();

		// 	has_exponent = true;
			
		// 	self.next_char();

		// 	match self.ch {
		// 		Some('+') | Some('-') => { let _ = self.next_char(); },
		// 		_ => {}
		// 	}

		// 	if !has_digit_before_exponent {
		// 		while self.ch.is_ascii_digit() {
		// 			self.next_char();
		// 		}

		// 		let end_pos = self.position;

		// 		return Err(LexicalError {
		// 			error: LexicalErrorType::MissingNumberBeforeExponent,
		// 			location: SrcSpan::from(start_pos, end_pos)
		// 		});
		// 	}

		// 	if self.ch.is_ascii_digit() {
		// 		while self.ch.is_ascii_digit() {
		// 			self.next_char();
		// 		}
		// 	} else {
		// 		let end_pos = self.position;

		// 		return Err(LexicalError {
		// 			error: LexicalErrorType::MissingDigitsAfterExponent,
		// 			location: SrcSpan::from(start_pos, end_pos)
		// 		});
		// 	}


		// 	break;
		// }

		// self.next_char();

		let end_pos = self.position;

		let last_char = value.chars().nth(value.len() - 1).unwrap();
		

		let expected_type = match last_char {
			'B' | 'b' => NumberType::Binary,
			'O' | 'o' => NumberType::Octal,
			'D' | 'd' => NumberType::Int,
			'H' | 'h' => NumberType::Hex,
			_ if has_period || has_exponent => {
				if has_exponent {
					if !has_number_before_exponent {
						return Err(LexicalError {
							error: LexicalErrorType::MissingNumberBeforeExponent,
							location: SrcSpan::from(start_pos, end_pos)
						});
					}
					if !has_number_after_exponent {
						return Err(LexicalError {
							error: LexicalErrorType::MissingDigitsAfterExponent,
							location: SrcSpan::from(start_pos, end_pos)
						});
					}
				}

				NumberType::Float
			},
			_ => NumberType::Int
		};

		if is_radix(last_char) {
			let _  = value.pop();
		}

		let token = match expected_type {
			NumberType::Binary => match i64::from_str_radix(&value, 2) {
				Ok(_) => Token::Binary(value),
				Err(_) => return Err(LexicalError {
					error: if !has_period { 
						LexicalErrorType::DigitOutOfRadix 
					} else { 
						LexicalErrorType::UnsupportedFloatingPoint 
					},
					location: SrcSpan::from(start_pos, end_pos)
				})
			},
			NumberType::Octal => match i64::from_str_radix(&value, 8) {
				Ok(_) => Token::Octal(value),
				Err(_) => return Err(LexicalError {
					error: if !has_period { 
						LexicalErrorType::DigitOutOfRadix 
					} else { 
						LexicalErrorType::UnsupportedFloatingPoint 
					},
					location: SrcSpan::from(start_pos, end_pos)
				})
			},
			NumberType::Hex => match i64::from_str_radix(&value, 16) {
				Ok(_) => Token::Hexadecimal(value),
				Err(_) => return Err(LexicalError {
					error: if !has_period { 
						LexicalErrorType::DigitOutOfRadix 
					} else { 
						LexicalErrorType::UnsupportedFloatingPoint 
					},
					location: SrcSpan::from(start_pos, end_pos)
				})
			},
			NumberType::Int => match i64::from_str_radix(&value, 10) {
				Ok(_) => Token::Int(value),
				Err(_) => return Err(LexicalError {
					error: LexicalErrorType::DigitOutOfRadix,
					location: SrcSpan::from(start_pos, end_pos)
				})
			},
			NumberType::Float => match value.parse::<f64>() {
				Ok(_) => Token::Float(value),
				Err(_) => return Err(LexicalError {
					error: LexicalErrorType::DigitOutOfRadix,
					location: SrcSpan::from(start_pos, end_pos)
				})
			}
		};

		Ok((start_pos as u32, token, end_pos as u32))
	}

	fn lex_comment(&mut self) -> Spanned {
		self.next_char();

		let start_pos = self.next_position;

		while (Some('*'), Some(')')) != (self.ch, self.next_ch) {
			let _ = self.next_char();
		}

		let end_pos = self.position;

		self.next_char(); // skip asterisk
		self.next_char(); // skip rparen

		(start_pos as u32, Token::Comment, end_pos as u32)
	}
}

impl<T: Iterator<Item = (u32, char)>> Iterator for Lexer<T> {
	type Item = LexResult;

	fn next(&mut self) -> Option<Self::Item> {
		let token = self.next_token();

		match token {
			Ok((_, Token::Eof, _)) => None,
			tok => Some(tok)
		}
	}
}