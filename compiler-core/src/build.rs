use std::{path::PathBuf, rc::Rc};

use crate::{analyzer::{ModuleAnalyzer, Outcome}, ast::Module, error::Error, parser::parse_module, warning::{TypeWarningEmitter, WarningEmitter, WarningEmitterIO}};

pub fn analyze(
    path: PathBuf,
    warnings: Rc<dyn WarningEmitterIO>,
) -> Result<Module, Error> {
    let warnings = WarningEmitter::new(warnings);

    let src = match std::fs::read_to_string(path.clone()) {
        Ok(src) => src,
        Err(err) => {
            let error = Error::StdIo { err: err.kind() };
            return Err(error)
        }
    };

    let parsed = match parse_module(&src) {
        Ok(parsed) => parsed,
        Err(err) => {
            // println!("{err:?}");
            let error = Error::Parse { path, src: src.to_string(), error: err };
            return Err(error)
        }
    };

    let warnings = TypeWarningEmitter::new(
        path.clone(),
        src.clone(),
        warnings
    );

    let outcome = ModuleAnalyzer::analyze(parsed.module, &warnings);

    match outcome {
        Outcome::Ok(module) => {
            Ok(module)
        },
        Outcome::PartialFailure(_, errors) => {
            // println!("{errors:?}");
            let error = Error::Type { path, src: src.to_string(), errors };
            Err(error)
        }
    }
}