mod error;
pub use error::{LexicalError, LexicalErrorType};

#[cfg(test)]
mod tests;

use std::fmt::Display;
use crate::token::Token;

pub type Spanned = (u32, Token, u32);
pub type LexResult = std::result::Result<Spanned, LexicalError>;

#[derive(Debug, PartialEq, Eq)]
pub struct SrcSpan {
    pub start: u32,
    pub end: u32,
}

impl SrcSpan {
	pub fn from_usize(start: usize, end: usize) -> Self {
		Self { start: start as u32, end: end as u32 }
	}
}

impl Display for SrcSpan {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}..{}", self.start, self.end)
	}
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
pub struct Lexer {
	position: usize,
	next_position: usize,
	ch: u8,
	input: Vec<u8>,
}

impl Display for Lexer {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "Lexer {{\n\tposition: {},\n\tnext_position: {},\n\tch: {},\n}}", self.position, self.next_position, String::from_utf8_lossy(&[self.ch]).to_string())
	}
}

pub fn str_to_keyword(word: &str) -> Option<Token> {
	Some(match word {
		"begin" => Token::Begin,
		"end" => Token::End,
		"for" => Token::For,
		"to" => Token::To,
		"do" => Token::Do,
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

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            position: 0,
            next_position: 0,
            ch: 0,
            input: input.into_bytes(),
        };

        lexer.next_char();

        return lexer;
    }

    pub fn next_token(&mut self) -> LexResult {
		self.skip_whitespace();

		let span = match self.ch {
			b'{' => self.eat_one_char(Token::LBrace),
			b'}' => self.eat_one_char(Token::RBrace),
			b'(' => {
				if self.peek() == b'*' {
					self.lex_comment()
				} else {
					self.eat_one_char(Token::LParen)
				}
			},
			b')' => self.eat_one_char(Token::RParen),
			b':' => {
				if self.peek() == b'=' {
					let start_pos = self.position;
					self.next_char();
					let end_pos = self.position;
					self.next_char();

					(start_pos as u32, Token::Assign, end_pos as u32)
				} else {
					self.eat_one_char(Token::Colon)
				}
			},
			b';' => self.eat_one_char(Token::Semicolon),
			b'!' => {
				if self.peek() == b'=' {
					let start_pos = self.position;
					self.next_char();
					let end_pos = self.position;
					self.next_char();

					(start_pos as u32, Token::NotEqual, end_pos as u32)
				} else {
					self.eat_one_char(Token::Bang)
				}
			},
			b'>' => {
				if self.peek() == b'=' {
					let start_pos = self.position;
					self.next_char();
					let end_pos = self.position;
					self.next_char();

					(start_pos as u32, Token::GreaterThanOrEqual, end_pos as u32)
				} else {
					self.eat_one_char(Token::GreaterThan)
				}
			},
			b'<' => {
				if self.peek() == b'=' {
					let start_pos = self.position;
					self.next_char();
					let end_pos = self.position;
					self.next_char();

					(start_pos as u32, Token::LessThanOrEqual, end_pos as u32)
				} else {
					self.eat_one_char(Token::LessThan)
				}
			},
			b'%' => self.eat_one_char(Token::Percent),
			b'@' => self.eat_one_char(Token::At),
			b'$' => self.eat_one_char(Token::Dollar),
			b'+' => self.eat_one_char(Token::Plus),
			b'-' => self.eat_one_char(Token::Minus),
			b'*' => self.eat_one_char(Token::Asterisk),
			b'/' => self.eat_one_char(Token::Slash),
			b'=' if self.peek() == b'=' => {
				let start_pos = self.position;
				self.next_char();
				let end_pos = self.position;
				self.next_char();

				(start_pos as u32, Token::Equal, end_pos as u32)
			},
			b'&' if self.peek() == b'&' => {
				let start_pos = self.position;
				self.next_char();
				let end_pos = self.position;
				self.next_char();

				(start_pos as u32, Token::And, end_pos as u32)
			},
			b'|' if self.peek() == b'|' => {
				let start_pos = self.position;
				self.next_char();
				let end_pos = self.position;
				self.next_char();

				(start_pos as u32, Token::Or, end_pos as u32)
			},
			b'"' => {
				let start_pos = self.position;
				self.next_char();
				let token = Token::String(self.lex_string());
				let end_pos = self.position;
				self.next_char();

				(start_pos as u32, token, end_pos as u32)
			},
			b'a'..=b'z' | b'A'..=b'Z' => {
				return self.lex_ident();
			},
			b'0'..=b'9' | b'.' => {
				return self.lex_number();
			},
			0 => self.eat_one_char(Token::Eof),
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
		};

		Ok(span)
    }

	fn peek(&self) -> u8 {
		if self.next_position >= self.input.len() {
			0
		} else {
			self.input[self.next_position]
		}
	}

    fn next_char(&mut self) {
		if self.next_position >= self.input.len() {
			self.ch = 0;
		} else {
			self.ch = self.input[self.next_position];
		}

		self.position = self.next_position;
		self.next_position += 1;
	}

	fn eat_one_char(&mut self, token: Token) -> Spanned {
		let start_pos = self.position;
		self.next_char();
		let end_pos = self.position;

		(start_pos as u32, token, end_pos as u32)
	}

    fn skip_whitespace(&mut self) {
		while self.ch.is_ascii_whitespace() {
			self.next_char();
		}
	}

	fn lex_ident(&mut self) -> LexResult {
        let start_pos = self.position;

		while self.ch.is_ascii_alphanumeric() {
			self.next_char();
		}

        let end_pos = self.position;

		let ident = String::from_utf8_lossy(&self.input[start_pos..end_pos])
			.to_string();

        if let Some(tok) = str_to_keyword(&ident) {
            Ok((start_pos as u32, tok, end_pos as u32))
        } else {
            Ok((start_pos as u32, Token::Ident(ident), end_pos as u32))
        }
	}

	fn lex_string(&mut self) -> String {
		let pos = self.position;

		while self.ch != b'"' {
			self.next_char();
		}

		String::from_utf8_lossy(&self.input[pos..self.position])
			.to_string()
	} 

	fn lex_number(&mut self) -> LexResult {
		let start_pos = self.position;

		let mut has_period = false;
		let mut has_hex = false;
		let mut has_exponent = false;

		fn is_ascii_hexalpha(ch: u8) -> bool { 
			match ch { 
				b'a'..=b'f' | b'A'..=b'F' => true, 
				_ => false 
			} 
		}

		fn is_exponent(ch: u8) -> bool {
			match ch {
				b'e' | b'E' => true,
				_ => false
			}
		}

		fn is_radix(ch: u8) -> bool {
			match ch {
				b'B' | b'b' => true,
				b'O' | b'o' => true,
				b'D' | b'd' => true,
				b'H' | b'h' => true,
				_ => false
			}
		}

		while self.ch.is_ascii_hexdigit() || self.ch == b'.' {
			// println!("{} -> {}", String::from_utf8_lossy(&[self.ch]).to_string(), String::from_utf8_lossy(&[self.peek()]).to_string());
			// println!("{}, !{}, {}", is_radix(self.ch), self.peek().is_ascii_hexdigit(), self.peek() != b'.');
			if is_radix(self.ch) && !self.peek().is_ascii_hexdigit() && self.peek() != b'.' {
				// println!("got radix");
				break;
			} else if !self.ch.is_ascii_hexdigit() && self.ch != b'.' {
				break;
			} else if has_period && self.ch == b'.' {
				self.next_char();

				while self.ch.is_ascii_digit() {
					self.next_char();
				}

				let end_pos = self.position;

				return Err(LexicalError {
					error: LexicalErrorType::MultipleFloatingPoints,
					location: SrcSpan::from_usize(start_pos, end_pos)
				});
			} else if !has_period && self.ch == b'.' {
				has_period = true;
			} else if !has_hex && is_ascii_hexalpha(self.ch) {
				has_hex = true;
			}

			if !has_exponent && is_exponent(self.ch) {
				let has_digit_before_exponent = self.input[self.position - 1]
					.is_ascii_hexdigit();

				has_exponent = true;
				
				self.next_char();

				match self.ch {
					b'+' | b'-' => self.next_char(),
					_ => {}
				}

				if !has_digit_before_exponent {
					while self.ch.is_ascii_digit() {
						self.next_char();
					}

					let end_pos = self.position;

					return Err(LexicalError {
						error: LexicalErrorType::MissingNumberBeforeExponent,
						location: SrcSpan::from_usize(start_pos, end_pos)
					});
				}

				if self.ch.is_ascii_digit() {
					while self.ch.is_ascii_digit() {
						self.next_char();
					}
				} else {
					let end_pos = self.position;

					return Err(LexicalError {
						error: LexicalErrorType::MissingDigitsAfterExponent,
						location: SrcSpan::from_usize(start_pos, end_pos)
					});
				}


				break;
			}

			self.next_char();
		}

		let end_pos = self.position;

		let value = String::from_utf8_lossy(&self.input[start_pos..end_pos])
			.to_string();

		// println!("{value}");

		let expected_type = match self.ch {
			b'B' | b'b' => NumberType::Binary,
			b'O' | b'o' => NumberType::Octal,
			b'D' | b'd' => NumberType::Int,
			b'H' | b'h' => NumberType::Hex,
			_ if has_period || has_exponent => NumberType::Float,
			_ => NumberType::Int
		};

		// println!("{expected_type:?}");

		if is_radix(self.ch) {
			self.next_char();
		}

		let token = match expected_type {
			NumberType::Binary => {
				if i64::from_str_radix(&value, 2).is_ok() && !has_period {
					Token::Binary(value)
				}
				else {
					return Err(LexicalError {
						error: if !has_period { 
							LexicalErrorType::DigitOutOfRadix 
						} else { 
							LexicalErrorType::UnsupportedFloatingPoint 
						},
						location: SrcSpan::from_usize(start_pos, end_pos)
					})
				}
			},
			NumberType::Octal => {
				if i64::from_str_radix(&value, 8).is_ok() && !has_period {
					Token::Octal(value)
				}
				else {
					return Err(LexicalError {
						error: if !has_period { 
							LexicalErrorType::DigitOutOfRadix 
						} else { 
							LexicalErrorType::UnsupportedFloatingPoint 
						},
						location: SrcSpan::from_usize(start_pos, end_pos)
					})
				}
			},
			NumberType::Hex => {
				if i64::from_str_radix(&value, 16).is_ok() && !has_period {
					Token::Hexadecimal(value)
				}
				else {
					return Err(LexicalError {
						error: if !has_period { 
							LexicalErrorType::DigitOutOfRadix 
						} else { 
							LexicalErrorType::UnsupportedFloatingPoint 
						},
						location: SrcSpan::from_usize(start_pos, end_pos)
					})
				}
			},
			NumberType::Int => {
				if i64::from_str_radix(&value, 10).is_ok() {
					Token::Int(value)
				}
				else {
					return Err(LexicalError {
						error: LexicalErrorType::DigitOutOfRadix,
						location: SrcSpan::from_usize(start_pos, end_pos)
					})
				}
			},
			NumberType::Float => {
				if value.parse::<f64>().is_ok() {
					Token::Float(value)
				}
				else {
					return Err(LexicalError {
						error: LexicalErrorType::DigitOutOfRadix,
						location: SrcSpan::from_usize(start_pos, end_pos)
					})
				}
			}
		};

		Ok((start_pos as u32, token, end_pos as u32))
	}

	fn lex_comment(&mut self) -> Spanned {
		self.next_char();

		let start_pos = self.next_position;

		while !(self.ch == b'*' && self.peek() == b')') {
			self.next_char();
		}

		let end_pos = self.position;
		let _ = String::from_utf8_lossy(&self.input[start_pos..end_pos])
			.to_string();

		self.next_char(); // skip asterisk
		self.next_char(); // skip rparen

		(start_pos as u32, Token::Comment, end_pos as u32)
	}
}

impl Iterator for Lexer {
	type Item = LexResult;

	fn next(&mut self) -> Option<Self::Item> {
		let token = self.next_token();

		match token {
			Ok((_, Token::Eof, _)) => None,
			tok => Some(tok)
		}
	}
}