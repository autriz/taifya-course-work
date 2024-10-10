use thiserror::Error;

use crate::{analyzer::error::Error as AnalyzerError, parser::ParseError};

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum Error {
    #[error("failed to parse source code")]
    Parse {
        src: String,
        error: ParseError
    },
    #[error("type checking failed")]
    Type {
        src: String,
        errors: Vec<AnalyzerError>
    },
    #[error("IO operation failed")]
    StdIo {
        err: std::io::ErrorKind
    }
}