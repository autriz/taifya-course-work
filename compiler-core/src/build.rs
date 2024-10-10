use std::path::PathBuf;

use crate::{analyzer::{ModuleAnalyzer, Outcome}, error::Error, parser::parse_module};

pub fn compile(
    path: PathBuf,
) -> Result<(), Error> {
    let src = match std::fs::read_to_string(path) {
        Ok(src) => src,
        Err(err) => {
            return Err(Error::StdIo { err: err.kind() })
        }
    };

    let parsed = match parse_module(&src) {
        Ok(parsed) => parsed,
        Err(err) => return Err(
            Error::Parse { src: src.to_string(), error: err }
        )
    };

    let outcome = ModuleAnalyzer::analyze(parsed.module);

    match outcome {
        Outcome::Ok(_) => {
            println!("analyzed successfuly!");
            Ok(())
        },
        Outcome::PartialFailure(_, errors) => {
            println!("got some yucky errors!");
            println!("{:?}", errors);
            Err(Error::Type { src: src.to_string(), errors })
        }
    }
}