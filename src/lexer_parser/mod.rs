use std::sync::Arc;

pub mod lexer;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SMLType {
    Char,
    String,
    Int,
    Bool,
    Regex,
    Function(Arc<[FunctionParam]>),
    Array(Box<SMLType>),

    Void,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionParam {
    ident: String,
    sml_type: SMLType,
}
