mod cli;
mod rlpl;
mod rppl;

use std::{cell::RefCell, path::PathBuf, rc::Rc};

use clap::Parser;
use cli::{print_analyzed, print_analyzing};
use lang_core::{build::compile, environment::Environment, eval::eval, warning::{Warning, WarningEmitterIO}};

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

            print_analyzing(path.to_str().unwrap());
            let start = std::time::Instant::now();

            match compile(path, warning_emitter.clone()) {
                Ok(module) => {
                    println!("{}", module.program);
                },
                Err(err) => {
                    err.pretty(&mut buf);
                    buf_writer
                        .print(&buf)
                        .expect("Writing warning to stderr");
                }
            };

            print_analyzed(std::time::Instant::now() - start);
        },
        Command::Run {
            path
        } => {
            let warning_emitter = Rc::new(ConsoleWarningEmitter);

            let buf_writer = crate::cli::stderr_buffer_writer();
            let mut buf = buf_writer.buffer();

            print_analyzing(path.to_str().unwrap());
            let start = std::time::Instant::now();

            match compile(path, warning_emitter.clone()) {
                Ok(module) => {
                    print_analyzed(std::time::Instant::now() - start);

                    let env = Rc::new(RefCell::new(Environment::new()));

                    eval(module, env);
                },
                Err(err) => {
                    err.pretty(&mut buf);
                    buf_writer
                        .print(&buf)
                        .expect("Writing warning to stderr");

                    print_analyzed(std::time::Instant::now() - start);
                }
            };
        },
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