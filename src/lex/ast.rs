use std::fmt::Display;

#[derive(Debug, Default)]
pub struct Mod {
    pub vals: Vec<Val>,
    pub types: Vec<Type>,
    pub fns: Vec<Fn>,
}

#[derive(Debug)]
pub struct Val {
    pub symbol: Identifier,
    pub type_: TypeExpression,
    pub value: Literal,
}

#[derive(Debug)]
pub struct Type {
    pub symbol: Identifier,
    pub type_: TypeExpression,
}

#[derive(Debug)]
pub struct Fn {
    pub symbol: Identifier,
    pub input: Vec<(Identifier, TypeExpression)>,
    pub output: TypeExpression,
    pub body: Expression,
}

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
    Identifier(Identifier),
    Compound(Vec<Expression>),
    Block(Vec<Expression>),
}

#[derive(Debug)]
pub enum TypeExpression {
    Identifier(Identifier),
    Compound(Vec<TypeExpression>)
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