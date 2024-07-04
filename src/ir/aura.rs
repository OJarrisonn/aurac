use defn::{FunctionDefinition, ValueDefinition};

pub mod ident;
pub mod type_;
pub mod value;
pub mod expr;
pub mod defn;

pub struct Module {
    pub kind: ModuleKind,
    pub name: String,
    pub vals: Vec<ValueDefinition>,
    pub fns: Vec<FunctionDefinition>,
}

pub enum ModuleKind {
    Lib,
    Bin,
}

#[derive(Debug, Default)]
pub enum SymbolAssocKind {
    #[default]
    Value,
    Method
}