mod error;
pub use error::{EvalError, EvalErrorType};

use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{Expression, Primitive, Statement},
    object::{
        environment::Environment, Object, Value, FALSE, NULL, TRUE
    }, 
    token::Token
};

pub fn eval(program: crate::ast::Program, env: Rc<RefCell<Environment>>) -> Object {
    let mut object = NULL;
    
    for statement in program.statements {
        object = eval_statement(statement, env.clone());
    }

    object
}

pub fn eval_statement(statement: crate::ast::Statement, env: Rc<RefCell<Environment>>) -> Object {
    match statement {
        Statement::Expression(expression) => eval_expression(expression, env),
        Statement::Program(program) => eval_program(program, env),
        Statement::Block(block) => eval_block(block, env),
        Statement::Declaration(declaration) => eval_declaration(declaration, env),
    }
}

fn eval_expression(expression: crate::ast::Expression, env: Rc<RefCell<Environment>>) -> Object {
    match expression {
        Expression::Primitive(primitive) => {
            match primitive {
                Primitive::Int { value, .. } => {
                    Object::Value(Value::Integer { value })
                },
                Primitive::Float { value, .. } => {
                    Object::Value(Value::Float { value })
                },
                Primitive::Hex { value, .. } => {
                    Object::Value(Value::Integer { value })
                },
                Primitive::Bin { value, .. } => {
                    Object::Value(Value::Integer { value })
                },
                Primitive::Octal { value, .. } => {
                    Object::Value(Value::Integer { value })
                },
                Primitive::String { value, .. } => {
                    Object::Value(Value::String { value })
                }
                Primitive::Bool { value, .. } => {
                    Object::Value(Value::Boolean { value })
                }
            }
        },
        Expression::Prefix(prefix) => {
            let right = eval_expression(*prefix.right, env);

            todo!();

            // eval_prefix_expression(prefix.operator, right)
        },
        Expression::Infix(infix) => {
            let left = eval_expression(*infix.left, env.clone());

            todo!();

            // eval_infix()
        },
        Expression::Conditional(conditional) => {
            todo!();

            // let condition = eval_infix(*conditional.condition, env.clone());

            // match condition {
                
            // }
        },
        Expression::ConditionalLoop(loop_) => {
            todo!()
        },
        Expression::FixedLoop(loop_) => {
            todo!()
        }
        Expression::Identifier(identifier) => {
            let val = env.borrow().get(&identifier.value);

            todo!();
        },
        // Expression::FunctionCall(func_call) => {
        //     let function = eval_expression(*func_call.function, env.clone());

        //     if is_error(&function) {
        //         return function;
        //     }

        //     let args = eval_expressions(func_call.arguments, env.clone());

        //     if args.len() == 1 && is_error(&args[0]) {
        //         return args[0].clone();
        //     }

        //     apply_function(function, args)
        // }
    }
}

fn eval_prefix_expression(operator: crate::token::Token, right: Object) -> Object {
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

fn eval_block(block: crate::ast::Block, env: Rc<RefCell<Environment>>) -> Object {
    let mut object = NULL;

    for expression in block.expressions {
        object = eval_expression(expression, env.clone());
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
    for name in declaration.names {
        env.borrow_mut().declare(
            name.value, 
            declaration.ident_type.clone().into()
        );
    }

    NULL
}

fn eval_assign() -> Object {
    todo!();

    NULL
}