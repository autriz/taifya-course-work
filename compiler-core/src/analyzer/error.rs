use crate::lexer::SrcSpan;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Problems {
    errors: Vec<Error>,
    warnings: Vec<Warning>,
}

impl Problems {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn error(&mut self, error: Error) {
        self.errors.push(error)
    }

    pub fn take_errors(&mut self) -> Vec<Error> {
        std::mem::take(&mut self.errors)
    }

    pub fn warning(&mut self, warning: Warning) {
        self.warnings.push(warning)
    }

    pub fn take_warnings(&mut self) -> Vec<Warning> {
        std::mem::take(&mut self.warnings)
    }

    pub fn sort(&mut self) {
        self.errors.sort_by_key(|e| e.start_location());
        self.warnings.sort_by_key(|w| w.location().start);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Error {
    TypeMismatch {
        location: SrcSpan
    },
    VariableNotDeclared {
        location: SrcSpan
    },
    InvalidUnaryOperation {
        location: SrcSpan
    }
}

impl Error {
    pub fn start_location(&self) -> u32 {
        match self {
            Error::TypeMismatch { location } |
            Error::VariableNotDeclared { location } |
            Error::InvalidUnaryOperation { location } => location.start
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Warning {
    UnusedVariable {
        location: SrcSpan
    },
    UnreachableElseClause {
        location: SrcSpan
    }
}

impl Warning {
    pub fn location(&self) -> SrcSpan {
        match self {
            Warning::UnusedVariable { location } |
            Warning::UnreachableElseClause { location } => *location
        }
    }
}