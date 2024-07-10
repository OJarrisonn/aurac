use std::fmt::Display;

#[derive(Debug, Default)]
pub struct Mod {
    pub vals: Vec<Val>,
}

#[derive(Debug)]
pub struct Val {
    pub symbol: Identifier,
    pub type_: TypeExpression,
    pub value: Literal,
}

#[derive(Debug)]
pub enum Identifier {
    Value(String),
    Type(String),
    Tag(String),
}

#[derive(Debug)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
}

#[derive(Debug)]
pub enum TypeExpression {
    Identifier(Identifier),
    Compound(Vec<TypeExpression>)
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Identifier::Value(s) | Identifier::Type(s) | Identifier::Tag(s) => write!(f, "{}", s),
        }
    }
}

impl From<i64> for Literal {
    fn from(i: i64) -> Self {
        Literal::Integer(i)
    }
}

impl From<f64> for Literal {
    fn from(f: f64) -> Self {
        Literal::Float(f)
    }
}

impl From<String> for Literal {
    fn from(s: String) -> Self {
        Literal::String(s)
    }
}