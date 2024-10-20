use std::{path::Path, process::Command};

use inkwell::{module::Module, targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine}, OptimizationLevel};

pub struct Compiler;
pub struct Linker;

impl Compiler {
    pub fn compile(
        opt_level: OptimizationLevel,
        reloc_mode: RelocMode,
        code_model: CodeModel,
        module: &Module,
        out: &str,
    ) {
        Target::initialize_all(&InitializationConfig::default());
        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple).unwrap();
        let target_machine = target.create_target_machine(
            &target_triple, 
            "generic", 
            "", 
            opt_level, 
            reloc_mode, 
            code_model
        ).unwrap();

        module.set_data_layout(&target_machine.get_target_data().get_data_layout());
        module.set_triple(&target_triple);
        
        // let buf = target_machine.write_to_memory_buffer(
        //     &module, 
        //     inkwell::targets::FileType::Object
        // ).unwrap();
        target_machine.write_to_file(
            &module, 
            inkwell::targets::FileType::Object, 
            Path::new(out)
        ).unwrap();
    }
}

impl Linker {
    pub fn link(input_file: &str, output_file: &str) -> Result<(), i32> {
        let gcc = Command::new("gcc")
            .arg(input_file)
            .arg("-o")
            .arg(output_file)
            .output()
            .expect("Unable to run `gcc` command");

        match gcc.status.success() {
            true => Ok(()),
            false => return Err(gcc.status.code().unwrap())
        }
    }
}