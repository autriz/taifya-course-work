use std::io::Write;

use lang_core::{lexer::Lexer, parser::Parser};

const PROMPT: &str = ">> ";

pub fn start() -> std::io::Result<()> {
	let stdin = std::io::stdin();
	
	loop {
		let mut input = String::from("");

		print!("{}", PROMPT);
		std::io::stdout().flush()?;
		stdin.read_line(&mut input)?;

		if let Some('\n') = input.chars().next_back() {
			input.pop();
		}
		if let Some('\r') = input.chars().next_back() {
			input.pop();
		}

		match input.as_str() {
			"" => {},
			".exit" => return Ok(()),
			_ => {
				let mut parser = Parser::new(Lexer::new(input.as_str()));

				match parser.parse() {
                    Ok(parsed) => {
                        for stmt in parsed.module.program.statements {
                            println!("{}", stmt);
                        }
                    },
                    Err(err) => {
                        let (message, messages) = err.details();

                        println!("Parse error: {}.\n\t{}", message, messages.join(";\n\t"))
                    }
                }
			}
		}
	}
}