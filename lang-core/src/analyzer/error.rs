use crate::{utils::prelude::SrcSpan, environment::prelude::ValueType};

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Problems {
    errors: Vec<AnalyzeError>,
    warnings: Vec<Warning>,
}

impl Problems {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn error(&mut self, error: AnalyzeError) {
        self.errors.push(error)
    }

    pub fn take_errors(&mut self) -> Vec<AnalyzeError> {
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
pub enum AnalyzeError {
    TypeMismatch {
        location: SrcSpan,
        expected: ValueType,
        got: ValueType,
    },
    VariableNotDeclared {
        location: SrcSpan,
        variable: String,
    },
    VariableNotInitialized {
        location: SrcSpan,
        variable: String,
    },
    VariableRedeclaration {
        location_a: SrcSpan,
        location_b: SrcSpan,
        variable: String,
    },
    InvalidUnaryOperation {
        location: SrcSpan
    },
    OperatorMismatch {
        location_a: SrcSpan,
        location_b: SrcSpan,
        expected: Vec<ValueType>,
        got_a: ValueType,
        got_b: ValueType
    }
}

impl AnalyzeError {
    pub fn start_location(&self) -> u32 {
        match self {
            AnalyzeError::TypeMismatch { location, .. }
            | AnalyzeError::VariableNotDeclared { location, .. }
            | AnalyzeError::VariableNotInitialized { location, .. }
            | AnalyzeError::VariableRedeclaration { location_b: location, .. }
            | AnalyzeError::InvalidUnaryOperation { location }
            | AnalyzeError::OperatorMismatch { location_a: location, .. } => location.start
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Warning {
    UnusedVariable {
        location: SrcSpan
    },
    EmptyDeclaration {
        location: SrcSpan
    },
    UnreachableIfClause {
        location: SrcSpan
    },
    UnreachableElseClause {
        location: SrcSpan
    },
    InfiniteLoop {
        location: SrcSpan
    },
    UnreachableWhileClause {
        location: SrcSpan
    }
}

impl Warning {
    pub fn location(&self) -> SrcSpan {
        match self {
            Warning::UnusedVariable { location, .. }
            | Warning::EmptyDeclaration { location }
            | Warning::UnreachableIfClause { location }
            | Warning::UnreachableElseClause { location }
            | Warning::InfiniteLoop { location }
            | Warning::UnreachableWhileClause { location } => *location
        }
    }
}