pub mod error;
use error::{Error, Problems};

#[cfg(test)]
mod tests;

use crate::{
    ast::{Expression, Identifier, Infix, Module, Operator, Prefix, Primitive, Statement}, 
    environment::Environment, 
    line_numbers::LineNumbers, 
    object::ValueType, 
    token::Token
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
        // line_numbers: LineNumbers, 
        // src_path: PathBuf
    ) -> Outcome<Module, Vec<Error>> {
        let mut analyzer = ModuleAnalyzer {
            problems: Default::default()
        };

        let mut env = Environment::new();

        let statements = module.program.statements.clone();
        let statements_count = statements.len();

        for statement in statements {
            analyzer.analyze_statement(statement, &mut env);
        }

        analyzer.problems.sort();

        env.convert_unused_to_warnings(&mut analyzer.problems);

        let warnings = analyzer.problems.take_warnings();
        for warning in &warnings {
            println!("{:?}", warning.clone());
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
                        println!("declare {}", name.value);
                        env.declare(
                            name.value.clone(), 
                            identifiers.names_type.to_owned().into()
                        );
                        env.init_usage(name.value, name.location, &mut self.problems);
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
                    self.problems.error(Error::TypeMismatch { location: value.location() })
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
                    self.problems.error(Error::TypeMismatch { location: conditional.condition.location() });
                }

                self.analyze_operator(*conditional.resolution, env);

                if let Some(alternative) = conditional.alternative {
                    self.analyze_operator(*alternative, env);
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
                    self.problems.error(Error::TypeMismatch { location: loop_.condition.location() });
                }

                self.analyze_operator(*loop_.block, env);
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
                    self.problems.error(Error::TypeMismatch { location: identifier.location });
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
                    self.problems.error(Error::TypeMismatch { location: value.location() });
                }

                let to_type = match get_expression_type(loop_.to.clone(), env) {
                    Ok(type_) => type_,
                    Err(err) => {
                        self.problems.error(err);
                        return;
                    }
                };

                if to_type != ValueType::Integer {
                    self.problems.error(Error::TypeMismatch { location: loop_.to.location() });
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
                        self.problems.error(Error::TypeMismatch { location: step.location() });
                    }
                }

                self.analyze_operator(*loop_.block, env);
            },
            Operator::Input(input) => {
                input.identifiers.iter().for_each(|ident| {
                    if let None = env.get(&ident.value) {
                        self.problems.error(Error::VariableNotDeclared { location: ident.location });
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
        None => Err(Error::VariableNotDeclared { location: identifier.location })
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
    let left_type = get_expression_type(*infix.left, env)?;
    let right_type = get_expression_type(*infix.right.clone(), env)?;

    if left_type == right_type {
        Ok(left_type)
    } else {
        Err(Error::TypeMismatch { location: infix.right.location() })
    }

    // check for invalid combinations
    // 1 + true
    // 1.5 * "hi"
    // 1.5 - 1
    // etc.
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