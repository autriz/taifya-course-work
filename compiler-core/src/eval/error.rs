use crate::lexer::SrcSpan;

pub enum EvalErrorType {

}

pub struct EvalError {
    error: EvalErrorType,
    location: SrcSpan
}