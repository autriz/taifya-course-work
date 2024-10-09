use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
enum Command {
    Analyze {
        path: PathBuf,
    },
    Run {
        path: PathBuf,
    }
}

fn main() {
    let result = match Command::parse() {
        Command::Analyze { 
            path 
        } => todo!("lexer -> parser -> analyzer"),
        Command::Run {
            path
        } => todo!("lexer -> parser -> analyzer -> eval")
    };
}
