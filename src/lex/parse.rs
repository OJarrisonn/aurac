use std::fmt::Display;

#[derive(Debug)]
pub struct Val {
    pub symbol: Identifier,
    pub type_: Identifier,
    pub value: i64,
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

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Identifier::Value(s) | Identifier::Type(s) | Identifier::Tag(s) => write!(f, "{}", s),
        }
    }
}