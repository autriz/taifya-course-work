use std::path::PathBuf;

use termcolor::Buffer;
use thiserror::Error;

use crate::{analyzer::error::Error as AnalyzerError, diagnostic::{Diagnostic, Label, Level, Location}, lexer::SrcSpan, parser::{ParseError, ParseErrorType}};

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum Error {
    #[error("failed to parse source code")]
    Parse {
        path: PathBuf,
        src: String,
        error: ParseError
    },
    #[error("type checking failed")]
    Type {
        path: PathBuf,
        src: String,
        errors: Vec<AnalyzerError>
    },
    #[error("IO operation failed")]
    StdIo {
        err: std::io::ErrorKind
    }
}

impl Error {
    pub fn pretty_string(&self) -> String {
        let mut nocolor = Buffer::no_color();
        self.pretty(&mut nocolor);
        String::from_utf8(nocolor.into_inner()).expect("Error printing produced invalid utf8")
    }

    pub fn pretty(&self, buf: &mut Buffer) {
        use std::io::Write;

        for diagnostic in self.to_diagnostics() {
            diagnostic.write(buf);
            writeln!(buf).expect("write new line diagnostic");
        }
    }

    pub fn to_diagnostics(&self) -> Vec<Diagnostic> {
        match self {
            Error::Parse { path, src, error } => {
                let (label, extra) = error.details();
                let text = extra.join("\n");

                let adjusted_location = if error.error == ParseErrorType::UnexpectedEof {
                    SrcSpan {
                        start: (src.len() - 1) as u32,
                        end: (src.len() - 1) as u32,
                    }
                } else {
                    error.span
                };

                vec![Diagnostic {
                    title: "Syntax error".into(),
                    text,
                    level: Level::Error,
                    location: Some(Location {
                        src: src.clone(),
                        path: path.clone(),
                        label: Label {
                            text: Some(label.to_string()),
                            span: adjusted_location,
                        },
                        extra_labels: vec![],
                    }),
                }]
            },
            Error::Type { path, src, errors } => {
                let mut diagnostics = vec![];

                errors
                    .iter()
                    .for_each(|error| {
                        match error {
                            AnalyzerError::InvalidUnaryOperation { location } => {
                                let text = format!("Invalid unary operation");

                                diagnostics.push(Diagnostic {
                                    title: "Type mismatch".into(),
                                    text,
                                    level: Level::Error,
                                    location: Some(Location {
                                        src: src.clone(),
                                        path: path.clone(),
                                        label: Label {
                                            text: None,
                                            span: *location,
                                        },
                                        extra_labels: vec![]
                                    }),
                                })
                            },
                            AnalyzerError::TypeMismatch { 
                                location, 
                                expected, 
                                got 
                            } => {
                                let text = format!("Expected `{expected:?}`, but got `{got:?}`");

                                diagnostics.push(Diagnostic {
                                    title: "Type mismatch".into(),
                                    text,
                                    level: Level::Error,
                                    location: Some(Location {
                                        src: src.clone(),
                                        path: path.clone(),
                                        label: Label {
                                            text: None,
                                            span: *location,
                                        },
                                        extra_labels: vec![]
                                    }),
                                })
                            },
                            AnalyzerError::VariableNotDeclared { location, variable } => {
                                let text = format!("Variable `{variable}` is not declared.");

                                diagnostics.push(Diagnostic {
                                    title: "Variable not declared".into(),
                                    text,
                                    level: Level::Error,
                                    location: Some(Location {
                                        src: src.clone(),
                                        path: path.clone(),
                                        label: Label {
                                            text: None,
                                            span: *location,
                                        },
                                        extra_labels: vec![]
                                    }),
                                })
                            },
                            AnalyzerError::VariableNotInitialized { location, variable } => {
                                let text = format!("Variable `{variable}` is not initialized.");

                                diagnostics.push(Diagnostic {
                                    title: "Variable not initialized".into(),
                                    text,
                                    level: Level::Error,
                                    location: Some(Location {
                                        src: src.clone(),
                                        path: path.clone(),
                                        label: Label {
                                            text: None,
                                            span: *location,
                                        },
                                        extra_labels: vec![]
                                    }),
                                })
                            }
                            AnalyzerError::VariableRedeclaration { 
                                location_a, 
                                location_b,
                                variable
                            } => {
                                let text = format!("Variable `{variable}` was declared multiple times.");

                                diagnostics.push(Diagnostic {
                                    title: "Multiple declarations".into(),
                                    text,
                                    level: Level::Error,
                                    location: Some(Location {
                                        src: src.clone(),
                                        path: path.clone(),
                                        label: Label {
                                            text: Some("Another defined here".into()),
                                            span: *location_b,
                                        },
                                        extra_labels: vec![Label {
                                            text: Some("First defined here".into()),
                                            span: *location_a
                                        }]
                                    }),
                                })
                            }
                            AnalyzerError::OperatorMismatch { 
                                location_a, 
                                location_b, 
                                expected, 
                                got_a, 
                                got_b 
                            } => {
                                let expected_types = expected.iter()
                                    .map(|type_| format!("{type_:?}"))
                                    .collect::<Vec<String>>()
                                    .join("`, `");

                                let text_a = format!("Expected any of `{expected_types}`, but got `{got_a:?}`");
                                let text_b = format!("Expected any of `{expected_types}`, but got `{got_b:?}`");

                                diagnostics.push(Diagnostic {
                                    title: "Type mismatch".into(),
                                    text: text_a,
                                    level: Level::Error,
                                    location: Some(Location {
                                        src: src.clone(),
                                        path: path.clone(),
                                        label: Label {
                                            text: None,
                                            span: *location_a,
                                        },
                                        extra_labels: vec![]
                                    }),
                                });
                                    
                                diagnostics.push(Diagnostic {
                                    title: "Type mismatch".into(),
                                    text: text_b,
                                    level: Level::Error,
                                    location: Some(Location {
                                        src: src.clone(),
                                        path: path.clone(),
                                        label: Label {
                                            text: None,
                                            span: *location_b,
                                        },
                                        extra_labels: vec![]
                                    }),
                                });
                            }
                        };
                    });

                diagnostics
            },
            Error::StdIo { err, } => {
                vec![Diagnostic {
                    title: "Standard IO error".into(),
                    text: format!("{err}"),
                    level: Level::Error,
                    location: None,
                }]
            }
    }}
}