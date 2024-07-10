#[derive(Debug, Clone)]
pub enum Identifier {
    Value(String),
    Type(String),
}

#[derive(Debug, Clone)]
pub enum Definition {
    Value(Identifier, TypeExpression, Literal),
    Type(Identifier, TypeExpression),
    Main(Vec<Statement>),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub value: Value,
    pub type_: Option<TypeExpression>,
}

#[derive(Debug, Clone)]
pub enum Value {
    Literal(Literal),
    Identifier(Identifier),
    Call(Identifier, Vec<(Option<Identifier>, Expression)>),
    Block(Vec<Statement>),
    Compound(Vec<Expression>),
    Struct(Vec<(Option<Identifier>, Expression)>),
    Function(Vec<(Identifier, TypeExpression)>, Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum TypeExpression {
    Identifier(Identifier),
    Function(FunctionTypeExpression),
    Compound(Vec<TypeExpression>),
    Struct(Vec<(Identifier, TypeExpression)>),
    Union(Vec<TypeExpression>),
    Enum(Vec<(Identifier, Option<TypeExpression>)>),
    Parametric(Identifier, Vec<TypeExpression>),
}

#[derive(Debug, Clone)]
pub struct FunctionTypeExpression {
    pub parameters: Vec<TypeExpression>,
    pub returns: Box<TypeExpression>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Bind(Identifier, Option<TypeExpression>, Expression),
    Expression(Expression),
}