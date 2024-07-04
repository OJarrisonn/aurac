use super::ident::Identifier;

#[derive(Debug)]
pub enum Value {
    Literal(ValueLiteral),
    Symbol(Identifier),
    Call(Call),
    Block(Vec<Statement>),
}

#[derive(Debug)]
pub enum Statement {
    Value(Value),
    Bind {
        ident: Identifier,
        value: Value,
    },
    Return(Value),
}

#[derive(Debug)]
pub struct Call {
    pub symbol: Identifier,
    pub args: Vec<Value>,
}

/// Represents a literal value in the Aura language.
#[derive(Debug)]
pub enum ValueLiteral {
    Int(i32),
    Float(f64),
    String(String),
    Bool(bool),
}

/// Represents a function literal in the Aura language. A closure.
#[derive(Debug)]
pub struct FunctionLiteral {
    pub args: Vec<Identifier>,
    pub captures: Vec<Identifier>,
    pub output: Value,
}