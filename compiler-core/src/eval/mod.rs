#[cfg(test)]
mod tests;

use std::{cell::RefCell, rc::Rc};

use crate::{
    environment::Environment, 
    object::{
        Object, Value, FALSE, NULL, TRUE
    }, 
    token::Token
};

pub fn eval(parsed: crate::ast::Parsed, env: Rc<RefCell<Environment>>) -> Object {
    let mut object = NULL;

    for statement in parsed.module.program.statements {
        object = eval_statement(statement, env.clone());
    }

    object
}

pub fn eval_statement(statement: crate::ast::Statement, env: Rc<RefCell<Environment>>) -> Object {
    todo!();
    // match statement {
    //     Statement::Expression(expression) => eval_expression(expression, env),
    //     Statement::Program(program) => eval_program(program, env),
    //     Statement::Block(block) => eval_block(block, env),
    //     Statement::Declaration(declaration) => eval_declaration(declaration, env),
    // }
}

fn eval_expression(expression: crate::ast::Expression, env: Rc<RefCell<Environment>>) -> Object {
    todo!();
    // match expression {
    //     Expression::Primitive(primitive) => {
    //         match primitive {
    //             Primitive::Int { value, .. } => {
    //                 Object::Value(Value::Integer { value })
    //             },
    //             Primitive::Float { value, .. } => {
    //                 Object::Value(Value::Float { value })
    //             },
    //             Primitive::Hex { value, .. } => {
    //                 Object::Value(Value::Integer { value })
    //             },
    //             Primitive::Bin { value, .. } => {
    //                 Object::Value(Value::Integer { value })
    //             },
    //             Primitive::Octal { value, .. } => {
    //                 Object::Value(Value::Integer { value })
    //             },
    //             Primitive::String { value, .. } => {
    //                 Object::Value(Value::String { value })
    //             }
    //             Primitive::Bool { value, .. } => {
    //                 Object::Value(Value::Boolean { value })
    //             }
    //         }
    //     },
    //     Expression::Prefix(prefix) => {
    //         let right = eval_expression(*prefix.right, env);

    //         eval_prefix(prefix.operator, right)
    //     },
    //     Expression::Infix(infix) => {
    //         // let left = eval_expression(*infix.left, env.clone());

    //         let operator = infix.operator;

    //         // let right = eval_expression(*infix.right, env.clone());

    //         // eval_infix(left, operator, right)
    //         todo!()
    //     },
    //     Expression::Assign(assign) => {
    //         let infix = assign.value;
    //         // let value = eval_infix(infix.left, infix.operator, infix.right);

    //         // env.borrow_mut().set(assign.identifier.value, value);

    //         NULL
    //     }
    //     Expression::Conditional(conditional) => {
    //         todo!();

    //         // let condition = eval_infix(*conditional.condition, env.clone());

    //         // match condition {
                
    //         // }
    //     },
    //     Expression::ConditionalLoop(loop_) => {
    //         todo!()
    //     },
    //     Expression::FixedLoop(loop_) => {
    //         todo!()
    //     }
    //     Expression::Identifier(identifier) => {
    //         let val = env.borrow().get(&identifier.value);

    //         todo!();
    //     },
    //     // Expression::FunctionCall(func_call) => {
    //     //     let function = eval_expression(*func_call.function, env.clone());

    //     //     if is_error(&function) {
    //     //         return function;
    //     //     }

    //     //     let args = eval_expressions(func_call.arguments, env.clone());

    //     //     if args.len() == 1 && is_error(&args[0]) {
    //     //         return args[0].clone();
    //     //     }

    //     //     apply_function(function, args)
    //     // }
    // }
}

fn eval_prefix(operator: crate::token::Token, right: Object) -> Object {
    match operator {
        Token::Bang => {
            match right {
                TRUE => FALSE,
                FALSE => TRUE,
                NULL => TRUE,
                Object::Value(Value::Integer { value }) => {
                    match value {
                        0 => TRUE,
                        _ => FALSE,
                    }
                },
                _ => Object::Error { message: format!("cannot apply unary operator `{}` to `{:?}`", operator.as_literal(), right._type()) },
            }
        },
        _ => Object::Error { message: format!("unknown unary operator `{}`", operator.as_literal()) }
    }
}

fn eval_infix(left: Object, operator: crate::token::Token, right: Object) -> Object {
    match (left, right) {
        (
            Object::Value(Value::Integer { value: left_value }), 
            Object::Value(Value::Integer { value: right_value })
        ) => {
            match operator {
                Token::Plus => Object::Value(Value::Integer { value: left_value + right_value }),
                Token::Minus => Object::Value(Value::Integer { value: left_value - right_value }),
                Token::Asterisk => Object::Value(Value::Integer { value: left_value * right_value }),
                Token::Slash => Object::Value(Value::Integer { value: left_value / right_value }),
                Token::LessThan => Object::Value(Value::Boolean { value: left_value < right_value }),
                Token::LessThanOrEqual => Object::Value(Value::Boolean { value: left_value <= right_value }),
                Token::GreaterThan => Object::Value(Value::Boolean { value: left_value > right_value }),
                Token::GreaterThanOrEqual => Object::Value(Value::Boolean { value: left_value >= right_value }),
                Token::Equal => Object::Value(Value::Boolean { value: left_value == right_value }),
                Token::NotEqual => Object::Value(Value::Boolean { value: left_value != right_value }),
                _ => unimplemented!("invalid operator for int int infix")
                // Object::Error(Error::from(
                //     format!("no implementation for `{}` {} `{}`", ObjectType::Integer, operator.as_literal(), ObjectType::Integer)
                // )),
            }
        },
        (
            Object::Value(Value::Float { value: left_value }), 
            Object::Value(Value::Float { value: right_value })
        ) => {
            match operator {
                Token::Plus => Object::Value(Value::Float { value: left_value + right_value }),
                Token::Minus => Object::Value(Value::Float { value: left_value - right_value }),
                Token::Asterisk => Object::Value(Value::Float { value: left_value * right_value }),
                Token::Slash => Object::Value(Value::Float { value: left_value / right_value }),
                Token::LessThan => Object::Value(Value::Boolean { value: left_value < right_value }),
                Token::LessThanOrEqual => Object::Value(Value::Boolean { value: left_value <= right_value }),
                Token::GreaterThan => Object::Value(Value::Boolean { value: left_value > right_value }),
                Token::GreaterThanOrEqual => Object::Value(Value::Boolean { value: left_value >= right_value }),
                Token::Equal => Object::Value(Value::Boolean { value: left_value == right_value }),
                Token::NotEqual => Object::Value(Value::Boolean { value: left_value != right_value }),
                _ => unimplemented!("invalid operator for float float infix")
                // Object::Error(Error::from(
                //     format!("no implementation for `{}` {} `{}`", ObjectType::Integer, operator.as_literal(), ObjectType::Integer)
                // )),
            }
        },
        (
            Object::Value(Value::String { value: left }), 
            Object::Value(Value::String { value: right })
        ) => {
            match operator {
                Token::Plus => Object::Value(Value::String { value: format!("{}{}", left, right)}),
                Token::Equal => Object::Value(Value::Boolean { value: left == right }),
                Token::NotEqual => Object::Value(Value::Boolean { value: left != right }),
                _ => unimplemented!("invalid operator for string string infix")
                // Object::Error(Error::from(
                //     format!("no implementation for `{}` {} `{}`", ObjectType::Integer, operator.as_literal(), ObjectType::Integer)
                // )),
            }
        }
        (
            Object::Value(Value::Boolean { value: left_value }), 
            Object::Value(Value::Boolean { value: right_value })
        ) => {
            match operator {
                Token::Equal => Object::Value(Value::Boolean { value: left_value == right_value }),
                Token::NotEqual => Object::Value(Value::Boolean { value: left_value != right_value }),
                _ => unimplemented!("invalid operator for boolean boolean infix")
                // Object::Error(Error::from(
                //     format!("no implementation for `{}` {} `{}`", ObjectType::Integer, operator.as_literal(), ObjectType::Integer)
                // )),
            }
        }
        (left, right) => {
            match operator {
                Token::Plus => {
                    Object::Error {
                        message: format!("cannot add `{:?}` to `{:?}`", left._type(), right._type())
                    }
                },
                Token::Minus => {
                    Object::Error {
                        message: format!("cannot subtract `{:?}` from `{:?}`", left._type(), right._type())
                    }
                },
                Token::Asterisk => {
                    Object::Error {
                        message: format!("cannot multiply `{:?}` by `{:?}`", left._type(), right._type())
                    }
                },
                Token::Slash => {
                    Object::Error {
                        message: format!("cannot divide `{:?}` by `{:?}`", left._type(), right._type())
                    }
                },
                Token::LessThan | 
                Token::GreaterThan | 
                Token::Equal | 
                Token::NotEqual | 
                Token::LessThanOrEqual |
                Token::GreaterThanOrEqual => {
                    Object::Error {
                        message: format!("mismatched types, expected `{:?}`, found `{:?}`", left._type(), right._type())
                    }
                },
                _ => Object::Error {
                    message: format!("expected operator, got `{}`", operator.as_literal())
                },
            }
        }
    }
}

fn eval_block(nested: crate::ast::Nested, env: Rc<RefCell<Environment>>) -> Object {
    let mut object = NULL;

    for operator in nested.operators {
        todo!();
        // object = eval_expression(expression, env.clone());
    }

    object
}

fn eval_program(program: crate::ast::Program, env: Rc<RefCell<Environment>>) -> Object {
    let mut object = NULL;

    for statement in program.statements {
        object = eval_statement(statement, env.clone());
    }

    object
}

fn eval_declaration(declaration: crate::ast::Declaration, env: Rc<RefCell<Environment>>) -> Object {
    for idents in declaration.identifiers {
        for name in idents.names {
            env.borrow_mut().declare(
                name.value, 
                idents.names_type.clone().into()
            );
        }
    }

    NULL
}

fn eval_assign() -> Object {
    todo!();

    NULL
}