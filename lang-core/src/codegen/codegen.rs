use std::collections::HashMap;

use inkwell::{basic_block::BasicBlock, builder::Builder, context::Context, module::Module, types::BasicTypeEnum, values::{BasicValue, BasicValueEnum, FunctionValue, InstructionOpcode}, AddressSpace, IntPredicate};
use super::variable::Variable;

use crate::{parser::prelude::{Declaration, Expression, IdentifierType, Operator, Primitive, Program, Statement}, lexer::prelude::Token};

fn printf_prototype<'ctx>(ctx: &'ctx Context, module: &Module<'ctx>) -> FunctionValue<'ctx> {
    let printf_type = ctx.i32_type().fn_type(
        &[
            ctx.ptr_type(AddressSpace::default()).into()
        ], 
        true
    );

    let printf = match module.get_function("printf") {
        Some(printf) => printf,
        None => {
            module.add_function("printf", printf_type, None)
        }
    };

    printf
}

fn scanf_prototype<'ctx>(ctx: &'ctx Context, module: &Module<'ctx>) -> FunctionValue<'ctx> {
    let scanf_type = ctx.i32_type().fn_type(
        &[
            ctx.ptr_type(AddressSpace::default()).into()
        ], 
        true
    );

    let scanf = match module.get_function("scanf") {
        Some(scanf) => scanf,
        None => {
            module.add_function("scanf", scanf_type, None)
        }
    };

    scanf
}

pub struct Codegen<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
    pub program: &'a Program,

    variables: HashMap<String, Variable<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,

    // built-in functions
    printf_fn: FunctionValue<'ctx>,
    scanf_fn: FunctionValue<'ctx>,
}

impl<'a, 'ctx> Codegen<'a, 'ctx> {
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }

    fn get_basic_type(&self, var_type: IdentifierType) -> BasicTypeEnum<'ctx> {
        match var_type {
            IdentifierType::Int => self.context.i64_type().into(),
            IdentifierType::Float => self.context.f64_type().into(),
            IdentifierType::Bool => self.context.bool_type().into(),
            IdentifierType::String => self.context.ptr_type(AddressSpace::default()).into()
        }
    }

    fn compile_stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Declaration(decl) => self.compile_declaration(decl),
            Statement::Operator(op) => self.compile_operator(op)
        }
    }

    fn compile_declaration(&mut self, declaration: &Declaration) {
        for identifiers in &declaration.identifiers {
            for name in &identifiers.names {
                let name = &name.value;
                let basic_type = self.get_basic_type(identifiers.names_type);

                let alloca = self.builder.build_alloca(basic_type, name)
                    .unwrap();

                self.variables.insert(
                    name.to_string(), 
                    Variable::new(
                        identifiers.names_type, 
                        basic_type, 
                        alloca
                    )
                );
            }
        }
    }

    fn compile_operator(&mut self, operator: &Operator) {
        match operator {
            Operator::Assignment(assign) => {
                let name = &assign.identifier.value;
                let value = self.compile_expression(&assign.value).unwrap();
                let variable = self.variables.get(name).expect("Variable should be defined");

                self.builder.build_store(variable.pointer, value).unwrap();
            },
            Operator::Nested(nested) => {
                if nested.operators.is_empty() {
                    return;
                }

                for operator in &nested.operators {
                    self.compile_operator(operator);
                }
            },
            Operator::Input(input) => {
                let mut identifiers = vec![];

                let formatted_string = input.identifiers.iter().map(|identifier| {
                    let variable = self.variables.get(&identifier.value)
                        .expect("Variable should be defined");

                    identifiers.push(variable.pointer.as_basic_value_enum().into());

                    match &variable.var_type {
                        IdentifierType::Int => "%ld",
                        IdentifierType::Float => "%lf",
                        IdentifierType::Bool => "%d",
                        IdentifierType::String => "%s"
                    }
                }).collect::<Vec<&str>>();

                let formatted_string = formatted_string.join(", ");

                let value = unsafe {
                    self.builder.build_global_string(&formatted_string, "").unwrap()
                };

                let mut args = vec![value.as_basic_value_enum().into()];
                args.append(&mut identifiers);

                let _ = self.builder.build_direct_call(
                    self.scanf_fn, 
                    &args, 
                    "scanf"
                ).unwrap();
            },
            Operator::Output(output) => {
                let mut expressions = vec![];

                let formatted_string = output.expressions.iter().map(|expression| {
                    let compiled_expr = self.compile_expression(expression)
                        .unwrap();

                    expressions.push(compiled_expr.into());

                    match &compiled_expr {
                        BasicValueEnum::IntValue(_) => "%ld",
                        BasicValueEnum::FloatValue(_) => "%lf",
                        BasicValueEnum::PointerValue(_) => "%s",
                        BasicValueEnum::ArrayValue(_) => "%s",
                        _ => unreachable!("Should not be an another basic type")
                    }
                }).collect::<Vec<&str>>();

                let formatted_string = format!("{}\n", formatted_string.join(""));

                let value = self.builder.build_global_string_ptr(
                    &formatted_string, 
                    "fmt_string"
                ).unwrap();

                let mut args = vec![value.as_pointer_value().into()];
                args.append(&mut expressions);

                let _ = self.builder.build_direct_call(
                    self.printf_fn, 
                    &args, 
                    "printf"
                ).unwrap();
            },
            Operator::Conditional(conditional) => {
                let parent = self.fn_value();
                let one_const = self.context.i64_type().const_int(1, false);
                let zero_const = self.context.i64_type().const_int(0, false);

                let mut incoming: Vec<(&dyn BasicValue<'ctx>, BasicBlock<'ctx>)> = vec![];

                let condition = self.compile_expression(&conditional.condition).unwrap();
                let condition = self.builder.build_int_compare(
                    IntPredicate::EQ, 
                    condition.into_int_value(), 
                    one_const, 
                    "if_cond"
                ).unwrap();

                let then_block = self.context.append_basic_block(parent, "if_then");
                let else_block = self.context.append_basic_block(parent, "if_else");
                let after_block = self.context.append_basic_block(parent, "if_after");

                self.builder.build_conditional_branch(condition, then_block, else_block).unwrap();
                
                self.builder.position_at_end(then_block);
                self.compile_operator(&conditional.resolution);
                self.builder.build_unconditional_branch(after_block).unwrap();

                let then_block = self.builder.get_insert_block().unwrap();
                
                incoming.push((&zero_const, then_block));

                self.builder.position_at_end(else_block);

                if conditional.alternative.is_some() {
                    self.compile_operator(&conditional.alternative.as_ref().unwrap());
                }

                self.builder.build_unconditional_branch(after_block).unwrap();
                
                let else_block = self.builder.get_insert_block().unwrap();
                
                incoming.push((&zero_const, else_block));
                self.builder.position_at_end(after_block);

                let phi = self.builder.build_phi(self.context.i64_type(), "if_block").unwrap();

                phi.add_incoming(incoming.as_slice());
            },
            Operator::ConditionalLoop(loop_) => {
                let parent = self.fn_value();
                let true_const = self.context.bool_type().const_int(1, false);
                
                let before_block = self.context.append_basic_block(parent, "while_before");
                let loop_block = self.context.append_basic_block(parent, "while");
                let after_block = self.context.append_basic_block(parent, "while_after");
                
                self.builder.build_unconditional_branch(before_block).unwrap();
                self.builder.position_at_end(before_block);

                let condition = self.compile_expression(&loop_.condition).unwrap();
                let condition = self.builder.build_int_compare(
                    IntPredicate::EQ, 
                    condition.into_int_value(), 
                    true_const, 
                    "while_cond"
                ).unwrap();

                self.builder.build_conditional_branch(
                    condition, 
                    loop_block, 
                    after_block
                ).unwrap();

                self.builder.position_at_end(loop_block);

                self.compile_operator(&loop_.block);

                if let Some(last_instruction) = loop_block.get_last_instruction() {
                    if last_instruction.get_opcode() != InstructionOpcode::Return {
                        self.builder.build_unconditional_branch(before_block).unwrap();
                    }
                }

                self.builder.position_at_end(after_block);
            },
            Operator::FixedLoop(loop_) => {
                let parent = self.fn_value();

                let name = &loop_.assignment.identifier.value;
                let value = self.compile_expression(&loop_.assignment.value).unwrap();
                let variable = self.variables.get(name).cloned().expect("Variable should be defined");
                let variable_type = self.get_basic_type(variable.var_type);

                self.builder.build_store(variable.pointer, value).unwrap();

                let loop_block = self.context.append_basic_block(parent, "for");
                let after_block = self.context.append_basic_block(parent, "for_after");

                self.builder.build_unconditional_branch(loop_block).unwrap();
                self.builder.position_at_end(loop_block);

                self.compile_operator(&loop_.block);

                let step = match &loop_.step {
                    Some(step) => self.compile_expression(step).unwrap().into_int_value(),
                    None => self.context.i64_type().const_int(1, false)
                };

                let to = self.compile_expression(&loop_.to).unwrap().into_int_value();

                let cur_value = self.builder.build_load(
                    variable_type, 
                    variable.pointer, 
                    &name
                ).unwrap();
                let next_value = self.builder.build_int_nsw_add(
                    cur_value.into_int_value(),
                    step,
                    "nextval"
                ).unwrap();

                self.builder.build_store(variable.pointer, next_value).unwrap();

                let end_condition = self.builder.build_int_compare(
                    IntPredicate::SLT,
                    cur_value.into_int_value(), 
                    to, 
                    "for_cond"
                ).unwrap();

                self.builder.build_conditional_branch(
                    end_condition, 
                    loop_block, 
                    after_block
                ).unwrap();
                self.builder.position_at_end(after_block);
            }
        }
    }

    fn compile_expression(&mut self, expression: &Expression) -> Result<BasicValueEnum<'ctx>, &'static str> {
        match expression {
            Expression::Identifier(ident) => {
                match self.variables.get(&ident.value) {
                    Some(variable) => Ok(
                        self.builder.build_load(
                            variable.basic_type, 
                            variable.pointer, 
                            &ident.value
                        ).unwrap()
                    ),
                    None => Err("Could not find a matching variable")
                }
            },
            Expression::Infix(infix) => {
                let left = self.compile_expression(&infix.left).unwrap();
                let right = self.compile_expression(&infix.right).unwrap();

                if left.get_type() != right.get_type() {
                    return Err("Value mismatch")
                }

                match left.get_type() {
                    BasicTypeEnum::IntType(_) => Ok({
                        match infix.operator {
                            Token::Plus => self.builder.build_int_add(
                                left.into_int_value(), 
                                right.into_int_value(), 
                                "intadd"
                            ).unwrap().into(),
                            Token::Minus => self.builder.build_int_sub(
                                left.into_int_value(), 
                                right.into_int_value(), 
                                "intsub"
                            ).unwrap().into(),
                            Token::Asterisk => self.builder.build_int_mul(
                                left.into_int_value(),
                                right.into_int_value(),
                                "intmul"
                            ).unwrap().into(),
                            Token::Slash => self.builder.build_int_signed_div(
                                left.into_int_value(),
                                right.into_int_value(),
                                "intdiv"
                            ).unwrap().into(),
                            Token::LessThan => self.builder.build_int_compare(
                                IntPredicate::SLT, 
                                left.into_int_value(),
                                right.into_int_value(),
                                "intcmp"
                            ).unwrap().into(),
                            Token::LessThanOrEqual => self.builder.build_int_compare(
                                IntPredicate::SLE, 
                                left.into_int_value(),
                                right.into_int_value(),
                                "intcmp"
                            ).unwrap().into(),
                            Token::GreaterThan => self.builder.build_int_compare(
                                IntPredicate::SGT, 
                                left.into_int_value(),
                                right.into_int_value(),
                                "intcmp"
                            ).unwrap().into(),
                            Token::GreaterThanOrEqual => self.builder.build_int_compare(
                                IntPredicate::SGE, 
                                left.into_int_value(),
                                right.into_int_value(),
                                "intcmp"
                            ).unwrap().into(),
                            Token::Equal => self.builder.build_int_compare(
                                IntPredicate::EQ, 
                                left.into_int_value(),
                                right.into_int_value(),
                                "intcmp"
                            ).unwrap().into(),
                            Token::NotEqual => self.builder.build_int_compare(
                                IntPredicate::NE, 
                                left.into_int_value(),
                                right.into_int_value(),
                                "intcmp"
                            ).unwrap().into(),
                            _ => return Err("Invalid operator")
                        }
                    }),
                    BasicTypeEnum::FloatType(_) => Ok({
                        match infix.operator {
                            Token::Plus => self.builder.build_int_add(
                                left.into_int_value(), 
                                right.into_int_value(), 
                                "intadd"
                            ).unwrap().into(),
                            Token::Minus => self.builder.build_int_sub(
                                left.into_int_value(), 
                                right.into_int_value(), 
                                "intsub"
                            ).unwrap().into(),
                            Token::Asterisk => self.builder.build_int_mul(
                                left.into_int_value(),
                                right.into_int_value(),
                                "intmul"
                            ).unwrap().into(),
                            Token::Slash => self.builder.build_int_signed_div(
                                left.into_int_value(),
                                right.into_int_value(),
                                "intdiv"
                            ).unwrap().into(),
                            Token::LessThan => self.builder.build_int_compare(
                                IntPredicate::SLT, 
                                left.into_int_value(),
                                right.into_int_value(),
                                "intcmp"
                            ).unwrap().into(),
                            Token::GreaterThan => self.builder.build_int_compare(
                                IntPredicate::SGT, 
                                left.into_int_value(),
                                right.into_int_value(),
                                "intcmp"
                            ).unwrap().into(),
                            _ => return Err("Invalid operator")
                        }
                    }),
                    _ => return Err("Invalid infix operation")
                }
            },
            Expression::Prefix(prefix) => {
                let op = &prefix.operator;
                let expr = self.compile_expression(&prefix.expression).unwrap();

                let value = match op {
                    Token::Bang => expr.into_int_value()
                        .const_neg()
                        .as_basic_value_enum(),
                    _ => return Err("Unexpected prefix operator"),
                };

                Ok(value)
            },
            Expression::Nested { expression, .. } => self.compile_expression(expression),
            Expression::Primitive(primitive) => match primitive {
                Primitive::Int { value, .. } => {
                    Ok(self.context.i64_type().const_int(*value as u64, false).as_basic_value_enum())
                },
                Primitive::Float { value, .. } => Ok(self.context.f64_type().const_float(*value).as_basic_value_enum()),
                Primitive::Bool { value: true, .. } => Ok(self.context.bool_type().const_int(1, false).as_basic_value_enum()),
                Primitive::Bool { value: false, .. } => Ok(self.context.bool_type().const_int(0, false).as_basic_value_enum()),
                Primitive::String { value, .. } => {
                    // let value = unsafe {
                    //     self.builder.build_global_string(value.as_str(), "").unwrap()
                    // };
                    let string = self.builder.build_global_string_ptr(
                        value, 
                        "str"
                    ).unwrap();
                    // let ptr = self.builder.build_alloca(
                    //     self.context.i8_type().array_type(value.len() as u32),
                    //     ""
                    // ).unwrap();
                    // self.builder.build_store(ptr, string).unwrap();

                    // self.builder.build_global_string_ptr(value, name)

                    Ok(string.as_pointer_value().into())
                    // Ok(ptr.as_basic_value_enum())
                }
            }
        }
    }

    fn compile_program(&mut self) -> Result<FunctionValue<'ctx>, &'static str> {
        let fn_type = self.context.i32_type().fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);

        if self.program.statements.is_empty() {
            return Ok(function);
        }

        let entry = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(entry);
        self.fn_value_opt = Some(function);

        for stmt in &self.program.statements {
            self.compile_stmt(stmt);
        }

        self.builder.build_return(Some(&self.context.i32_type().const_zero())).unwrap();

        if function.verify(true) {
            Ok(function)
        } else {
            unsafe { function.delete(); }

            Err("Invalid function")
        }
    }

    pub fn compile(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
        program: &'a Program,
    ) -> Result<FunctionValue<'ctx>, &'static str> {
        let printf_fn = printf_prototype(context, module);
        let scanf_fn = scanf_prototype(context, module);

        let mut codegen = Codegen {
            context,
            builder,
            module,
            program,
            variables: HashMap::new(),
            fn_value_opt: None,
            printf_fn,
            scanf_fn
        };

        codegen.compile_program()
    }
}

#[cfg(test)]
mod test {
    use std::path::Path;

    use inkwell::{context::Context, targets::{InitializationConfig, Target, TargetMachine}};

    use crate::parser::prelude::parse_module;

    use super::Codegen;

    #[test]
    fn test_compile() {
        let src = r#"
            begin
                var e: @;;

                e := "Hi";

                writeln "Value: ", e
            end
        "#;
        // let src = r#"
        //     begin
        //         var i: %;;

        //         i := 0;

        //         while (i < 5) i := i + 1;

        //         writeln i
        //     end
        // "#;

        let parsed = parse_module(src).unwrap();

        let context = Context::create();
        let builder = context.create_builder();
        let module = context.create_module("test_compile");

        let _compiled = Codegen::compile(
            &context, 
            &builder, 
            &module, 
            &parsed.module.program
        ).unwrap();

        Target::initialize_all(&InitializationConfig::default());
        let target_triple = TargetMachine::get_default_triple();
        // println!("{target_triple:?}");
        let target = Target::from_triple(&target_triple).unwrap();
        // println!("{target:?}");
        let target_machine = target.create_target_machine(
            &target_triple, 
            "generic", 
            "", 
            inkwell::OptimizationLevel::None, 
            inkwell::targets::RelocMode::PIC, 
            inkwell::targets::CodeModel::Default
        ).unwrap();
        // println!("{target_machine:?}");

        module.set_data_layout(&target_machine.get_target_data().get_data_layout());
        module.set_triple(&target_triple);
        
        let out_file = "hohiho.o";
        // let buf = target_machine.write_to_memory_buffer(
        //     &module, 
        //     inkwell::targets::FileType::Object
        // ).unwrap();
        target_machine.write_to_file(
            &module, 
            inkwell::targets::FileType::Object, 
            Path::new(&out_file.to_string())
        ).unwrap();

        println!("{}", module.print_to_string().to_string());
    }
}