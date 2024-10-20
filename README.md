# Taifya-course-work

This project was made as a course work for subject "Theory of automatas and formal languages".

Project - is a compiler for a programming language, and it consists of 3 elements:
* Lexical analyzer
* Syntactical analyzer
* Semantical analyzer

### How to run
* Clone repository
* [Download Rust](https://rustup.rs/)
* [Download LLVM 18.1.8](https://github.com/llvm/llvm-project/releases/tag/llvmorg-18.1.8) (clang+llvm is recommended, but llvm only should be enough)
* Read [llvm-sys](https://docs.rs/crate/llvm-sys/180.0.0) build requirements how to make LLVM work
* Type `cargo run` in the repository folder