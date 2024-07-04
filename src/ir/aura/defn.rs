use super::{expr::{Value, ValueLiteral}, ident::Identifier, type_::TypeExpression};

#[derive(Debug)]
pub struct TypeDefinition {
    pub name: Identifier,
    pub type_: TypeExpression,
}

/// A symbol thats being defined at the moment. The type must be informed for the new symbol.
/// 
/// Those symbols are bound to the current scope.
/// 
/// ```ignore
/// name Type
/// ```
#[derive(Debug, Default)]
pub struct SymbolDefinition {
    pub type_: TypeExpression,
    pub name: Identifier,
}

/// Represents a `val` static statement in the Aura language.
/// 
/// ```ignore
/// val number Int = 10
/// ```
#[derive(Debug)]
pub struct ValueDefinition {
    pub symbol: SymbolDefinition,
    pub value: ValueLiteral,
}

/// Represents a `fn` static statement in the Aura language.
#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: Identifier,
    pub args: Vec<SymbolDefinition>,
    pub output: TypeExpression,
    pub body: Value,
}