mod rlpl;
mod rppl;

use std::path::PathBuf;

use clap::Parser;
use lang_core::build::compile;

#[derive(Parser)]
enum Command {
    Analyze {
        path: PathBuf,
    },
    Run {
        path: PathBuf,
    },
    Rlpl,
    Rppl
}

fn main() {
    let _ = match Command::parse() {
        Command::Analyze { path } => {
            match compile(path) {
                Ok(_) => {},
                Err(err) => {
                    println!("{err}")
                }
            };
        },
        Command::Run {
            path
        } => todo!("lexer -> parser -> analyzer -> eval"),
        Command::Rlpl => {
            let _  = rlpl::start();
        },
        Command::Rppl => {
            let _ = rppl::start();
        }
    };
}
