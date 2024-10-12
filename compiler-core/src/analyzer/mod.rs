pub mod error;

use error::{Error, Problems, Warning};

#[cfg(test)]
mod tests;

use crate::{
    ast::{Expression, Identifier, Infix, Module, Operator, Prefix, Primitive, Statement}, environment::Environment, lexer::SrcSpan, value::ValueType, token::Token, warning::TypeWarningEmitter
};

pub enum Outcome<T, E> {
    Ok(T),
    PartialFailure(T, E)
}

#[derive(Debug)]
pub struct ModuleAnalyzer {
    problems: Problems,
}

impl ModuleAnalyzer {
    pub fn analyze(
        module: Module, 
        warnings: &TypeWarningEmitter
    ) -> Outcome<Module, Vec<Error>> {
        let mut analyzer = ModuleAnalyzer {
            problems: Default::default()
        };

        let mut env = Environment::new();

        let statements = module.program.statements.clone();

        for statement in statements {
            analyzer.analyze_statement(statement, &mut env);
        }

        env.convert_unused_to_warnings(&mut analyzer.problems);
        
        analyzer.problems.sort();

        let warning_list = analyzer.problems.take_warnings();
        for warning in &warning_list {
            warnings.emit(warning.clone());
        }

        match Vec::try_from(analyzer.problems.take_errors()) {
            Err(_) => Outcome::Ok(module),
            Ok(errors) if errors.len() > 0 => Outcome::PartialFailure(module, errors),
            _ => Outcome::Ok(module)
        }
    }

    fn analyze_statement(&mut self, statement: Statement, env: &mut Environment) {
        match statement {
            Statement::Declaration(declaration) => {
                for identifiers in declaration.identifiers {
                    for name in identifiers.names {
                        // println!("declare {}", name.value);
                        match env.get(&name.value) {
                            None => {
                                env.declare(
                                    name.value.clone(), 
                                    identifiers.names_type.to_owned().into()
                                );
                                env.init_usage(name.value, name.location, &mut self.problems);
                            }
                            Some(_) => {
                                self.problems.error(Error::VariableRedeclaration { 
                                    location_a: env.usages.get(&name.value).unwrap().0, 
                                    location_b: name.location,
                                    variable: name.value
                                })
                            }
                        }
                    }
                }
            },
            Statement::Operator(operator) => self.analyze_operator(operator, env)
        }
    }

    fn analyze_operator(&mut self, operator: Operator, env: &mut Environment) {
        match operator {
            Operator::Assignment(assignment) => {
                let identifier = assignment.identifier.clone();
                let identifier_type = match get_identifier_type(identifier, env) {
                    Ok(type_) => type_,
                    Err(err) => {
                        self.problems.error(err);
                        return;
                    }
                };
                env.increment_usage(&assignment.identifier.value);

                let value = assignment.value;
                let value_type = match get_expression_type(value.clone(), env) {
                    Ok(type_) => type_,
                    Err(err) => {
                        self.problems.error(err);
                        return;
                    }
                };
                
                if identifier_type != value_type {
                    self.problems.error(Error::TypeMismatch { 
                        location: value.location(),
                        expected: identifier_type,
                        got: value_type
                    })
                }
            },
            Operator::Nested(nested) => {
                nested.operators.into_iter().for_each(|operator| {
                    self.analyze_operator(operator, env);
                });
            },
            Operator::Conditional(conditional) => {
                let condition_type = match get_expression_type(conditional.condition.clone(), env) {
                    Ok(type_) => type_,
                    Err(err) => {
                        self.problems.error(err);
                        return;
                    }
                };

                if condition_type != ValueType::Boolean {
                    self.problems.error(Error::TypeMismatch { 
                        location: conditional.condition.location(),
                        expected: ValueType::Boolean,
                        got: condition_type
                    });
                }

                self.analyze_operator(*conditional.resolution.clone(), env);

                if let Some(alternative) = conditional.alternative.clone() {
                    self.analyze_operator(*alternative, env);
                }

                match conditional.condition {
                    Expression::Primitive(Primitive::Bool { value, .. }) => match value {
                        true => { 
                            if let Some(alternative) = conditional.alternative {
                                self.problems.warning(Warning::UnreachableElseClause { location: alternative.location() }); 
                            }
                        },
                        false => {
                            self.problems.warning(Warning::UnreachableIfClause { location: conditional.resolution.location() })
                        }
                    },
                    _ => {}
                }
            },
            Operator::ConditionalLoop(loop_) => {
                let condition_type = match get_expression_type(loop_.condition.clone(), env) {
                    Ok(type_) => type_,
                    Err(err) => {
                        self.problems.error(err);
                        return;
                    }
                };

                if condition_type != ValueType::Boolean {
                    self.problems.error(Error::TypeMismatch { 
                        location: loop_.condition.location(),
                        expected: ValueType::Boolean,
                        got: condition_type
                    });
                }

                self.analyze_operator(*loop_.block.clone(), env);

                match loop_.condition {
                    Expression::Primitive(Primitive::Bool { value, .. }) => match value {
                        true => { 
                            let start = loop_.location.start;
                            let end = loop_.condition.location().end + 1;

                            self.problems.warning(Warning::InfiniteLoop {  location: SrcSpan { start, end } }); 
                        },
                        false => {
                            self.problems.warning(Warning::UnreachableWhileClause { location: loop_.block.location() });
                        }
                    },
                    _ => {}
                }
            },
            Operator::FixedLoop(loop_) => {
                let assignment = loop_.assignment;

                let identifier = assignment.identifier;
                let identifier_type = match get_identifier_type(identifier.clone(), env) {
                    Ok(type_) => type_,
                    Err(err) => {
                        self.problems.error(err);
                        return;
                    }
                };

                if identifier_type != ValueType::Integer {
                    self.problems.error(Error::TypeMismatch { 
                        location: identifier.location,
                        expected: identifier_type,
                        got: ValueType::Integer
                    });
                }

                let value = assignment.value;
                let value_type = match get_expression_type(value.clone(), env) {
                    Ok(type_) => type_,
                    Err(err) => {
                        self.problems.error(err);
                        return;
                    }
                };

                if value_type != ValueType::Integer {
                    self.problems.error(Error::TypeMismatch { 
                        location: value.location(),
                        expected: ValueType::Integer,
                        got: value_type 
                    });
                }

                let to_type = match get_expression_type(loop_.to.clone(), env) {
                    Ok(type_) => type_,
                    Err(err) => {
                        self.problems.error(err);
                        return;
                    }
                };

                if to_type != ValueType::Integer {
                    self.problems.error(Error::TypeMismatch { 
                        location: loop_.to.location(),
                        expected: ValueType::Integer,
                        got: to_type
                    });
                }

                if let Some(step) = loop_.step {
                    let step_type = match get_expression_type(step.clone(), env) {
                        Ok(type_) => type_,
                        Err(err) => {
                            self.problems.error(err);
                            return;
                        }
                    };

                    if step_type != ValueType::Integer {
                        self.problems.error(Error::TypeMismatch { 
                            location: step.location(),
                            expected: ValueType::Integer,
                            got: step_type
                        });
                    }
                }

                self.analyze_operator(*loop_.block, env);
            },
            Operator::Input(input) => {
                input.identifiers.iter().for_each(|ident| {
                    if let None = env.get(&ident.value) {
                        self.problems.error(Error::VariableNotDeclared { 
                            location: ident.location,
                            variable: ident.value.clone()
                        });
                    }
                });
            },
            Operator::Output(output) => {
                output.expressions.into_iter().for_each(|expr| {
                    self.analyze_expression(expr, env);
                });
            }
        }
    }

    fn analyze_expression(&self, expression: Expression, env: &mut Environment) {
        let _ = get_expression_type(expression, env);
    }
}

fn get_expression_type(
    expression: Expression, 
    env: &mut Environment
) -> Result<ValueType, Error> {
    match expression {
        Expression::Identifier(identifier) => {
            env.increment_usage(&identifier.value);

            get_identifier_type(identifier, env)
        },
        Expression::Primitive(primitive) => Ok(get_primitive_type(primitive)),
        Expression::Infix(infix) => get_infix_type(infix, env),
        Expression::Prefix(prefix) => get_prefix_type(prefix, env),
        Expression::Nested { expression, .. } => get_expression_type(*expression, env)
    }
}

fn get_identifier_type(
    identifier: Identifier, 
    env: &Environment
) -> Result<ValueType, Error> {
    match env.get(&identifier.value) {
        Some(value) => Ok(value._type()),
        None => Err(Error::VariableNotDeclared {
            location: identifier.location,
            variable: identifier.value
        })
    }
}

fn get_primitive_type(primitive: Primitive) -> ValueType {
    match primitive {
        Primitive::Int { .. } => ValueType::Integer,
        Primitive::Float { .. } => ValueType::Float,
        Primitive::String { .. } => ValueType::String,
        Primitive::Bool { .. } => ValueType::Boolean,
    }
}

fn get_infix_type(
    infix: Infix, 
    env: &mut Environment
) -> Result<ValueType, Error> {
    let left_type = get_expression_type(*infix.left.clone(), env)?;
    let right_type = get_expression_type(*infix.right.clone(), env)?;

    let allowed_types = get_allowed_types_for(&infix.operator);

    let is_left_allowed = allowed_types.contains(&left_type);
    let is_right_allowed = allowed_types.contains(&right_type);

    let value_type = match (is_left_allowed, is_right_allowed) {
        (true, true) => match (&left_type, &right_type) {
            (ValueType::Integer, ValueType::Integer)
            | (ValueType::Float, ValueType::Float)
            | (ValueType::Boolean, ValueType::Boolean)
            | (ValueType::String, ValueType::String) => left_type, 
            _ => return Err(Error::TypeMismatch { 
                location: infix.right.location(), 
                expected: left_type, 
                got: right_type 
            })
        },
        _ => return Err(Error::OperatorMismatch { 
            location_a: infix.left.location(), 
            location_b: infix.right.location(), 
            expected: allowed_types, 
            got_a: left_type, 
            got_b: right_type 
        })
    };

    let value_type = match value_type {
        ValueType::Integer => match infix.operator {
            Token::LessThan
            | Token::LessThanOrEqual
            | Token::GreaterThan
            | Token::GreaterThanOrEqual
            | Token::Equal
            | Token::NotEqual => ValueType::Boolean,
            _ => value_type
        },
        ValueType::Float => match infix.operator {
            Token::LessThan
            | Token::GreaterThan => ValueType::Boolean,
            _ => value_type
        },
        ValueType::String => match infix.operator {
            Token::Equal
            | Token::NotEqual => ValueType::Boolean,
            _ => value_type
        },
        ValueType::Boolean => value_type
    };

    Ok(value_type)
}

fn get_prefix_type(
    prefix: Prefix,
    env: &mut Environment
) -> Result<ValueType, Error> {
    let expression_type = get_expression_type(*prefix.expression, env)?;

    match (prefix.operator, &expression_type) {
        (Token::Bang, ValueType::Boolean) => Ok(expression_type),
        _ => Err(Error::InvalidUnaryOperation { location: prefix.location })
    }
}

fn get_allowed_types_for(operator: &Token) -> Vec<ValueType> {
    if !operator.is_operator() {
        return vec![];
    }

    match operator {
        Token::Plus => vec![ValueType::Integer, ValueType::Float, ValueType::String],
        Token::Minus
        | Token::Asterisk
        | Token::Slash => vec![ValueType::Integer, ValueType::Float],
        Token::And
        | Token::Or => vec![ValueType::Boolean],
        Token::LessThan
        | Token::GreaterThan => vec![ValueType::Integer, ValueType::Float],
        Token::LessThanOrEqual
        | Token::GreaterThanOrEqual => vec![ValueType::Integer],
        Token::Equal
        | Token::NotEqual => vec![ValueType::Integer, ValueType::Boolean, ValueType::String],
        _ => unreachable!("This should not match")
    }
}