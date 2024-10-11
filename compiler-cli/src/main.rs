mod cli;
mod rlpl;
mod rppl;

use std::{path::PathBuf, rc::Rc};

use clap::Parser;
use cli::{print_compiled, print_compiling};
use lang_core::{build::compile, warning::{Warning, WarningEmitterIO}};

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
            let warning_emitter = Rc::new(ConsoleWarningEmitter);

            let buf_writer = crate::cli::stderr_buffer_writer();
            let mut buf = buf_writer.buffer();

            print_compiling(path.to_str().unwrap());
            let start = std::time::Instant::now();

            match compile(path, warning_emitter.clone()) {
                Ok(_) => {},
                Err(err) => {
                    err.pretty(&mut buf);
                    buf_writer
                        .print(&buf)
                        .expect("Writing warning to stderr");
                }
            };

            print_compiled(std::time::Instant::now() - start);
        },
        Command::Run {
            path: _path
        } => todo!("lexer -> parser -> analyzer -> eval"),
        Command::Rlpl => {
            let _  = rlpl::start();
        },
        Command::Rppl => {
            let _ = rppl::start();
        }
    };
}

#[derive(Debug, Clone, Copy)]
pub struct ConsoleWarningEmitter;

impl WarningEmitterIO for ConsoleWarningEmitter {
    fn emit_warning(&self, warning: Warning) {
        let buffer_writer = crate::cli::stderr_buffer_writer();
        let mut buffer = buffer_writer.buffer();
        warning.pretty(&mut buffer);
        buffer_writer
            .print(&buffer)
            .expect("Writing warning to stderr");
    }
}