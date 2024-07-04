use super::{defn::SymbolDefinition, ident::Identifier};

#[derive(Debug, Default)]
pub enum TypeExpression {
    Struct(Vec<SymbolDefinition>),
    Enum(Vec<SymbolDefinition>),
    Compound(Vec<Self>),
    Ident(Identifier),
    Functional(Box<Self>, Box<Self>),
    #[default]
    Void,
}