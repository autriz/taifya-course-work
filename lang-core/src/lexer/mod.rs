pub mod error;
pub mod token;
pub mod lexer;

pub mod prelude {
	pub use super::{
		error::*,
		token::*,
		lexer::*
	};
}

#[cfg(test)]
mod tests;