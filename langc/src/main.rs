mod cli;
mod rlpl;
mod rppl;
mod compiler;
use compiler::{ObjectCompiler, ObjectLinker};

use std::{cell::RefCell, path::PathBuf, rc::Rc};

use clap::Parser;
use cli::{print_analyzed, print_analyzing};
use inkwell::context::Context;
use lang_core::{
    analyzer::analyze, 
    codegen::prelude::Codegen,
    environment::prelude::Environment, 
    eval::eval, 
    utils::prelude::{Warning, WarningEmitterIO}
};

#[derive(Parser)]
enum Command {
    Analyze {
        path: PathBuf,
    },
    Run {
        path: PathBuf,
    },
    Compile {
        path: PathBuf,
        output: Option<PathBuf>,
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

            match analyze(path, warning_emitter.clone()) {
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

            match analyze(path, warning_emitter.clone()) {
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
        Command::Compile { 
            path, 
            output 
        } => {
            let warning_emitter = Rc::new(ConsoleWarningEmitter);

            let buf_writer = crate::cli::stderr_buffer_writer();
            let mut buf = buf_writer.buffer();

            print_analyzing(path.to_str().unwrap());
            let start = std::time::Instant::now();

            let analyzed = match analyze(path.clone(), warning_emitter.clone()) {
                Ok(module) => module,
                Err(err) => {
                    err.pretty(&mut buf);
                    buf_writer
                        .print(&buf)
                        .expect("Writing warning to stderr");

                    return;
                }
            };

            print_analyzed(std::time::Instant::now() - start);

            let context = Context::create();
            let builder = context.create_builder();
            let module = context.create_module(path.clone().file_name().unwrap().to_str().unwrap());

            let _ = Codegen::compile(
                &context,
                &builder,
                &module,
                &analyzed.program
            );

            let output = match output {
                Some(output) => output,
                None => PathBuf::from(path.file_name().unwrap().to_str().unwrap())
            };

            let mut obj_file = output.clone();
            obj_file.set_extension("o");
            let obj_file = obj_file.as_os_str().to_str().unwrap();

            ObjectCompiler::compile(
                inkwell::OptimizationLevel::Default, 
                inkwell::targets::RelocMode::PIC, 
                inkwell::targets::CodeModel::Default, 
                &module, 
                &obj_file
            );

            let mut exe_file = output.clone();
            exe_file.set_extension("");
            let exe_file = exe_file.as_os_str().to_str().unwrap();

            ObjectLinker::link(
                obj_file,
                exe_file
            ).unwrap();

            let _ = std::fs::remove_file(obj_file);
        }
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