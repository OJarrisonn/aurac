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

/// A symbol identifier.
/// 
/// Symbols may belong to a type
/// 
/// - `print` a symbol
/// - `String:new` a method in the type `String`
/// - `Vec.max_size` a value in the type `Vec`
#[derive(Debug, Default)]
pub struct SymbolIdentifier {
    pub type_: Option<TypeIdentifier>,
    pub assoc_kind: Option<SymbolAssocKind>,
    pub name: String,
}

/// A type identifier.
#[derive(Debug, Default)]
pub struct TypeIdentifier {
    pub name: String,
}

/// A symbol thats being defined at the moment. The type must be informed for the new symbol.
/// 
/// Those symbols are bound to the current scope.
#[derive(Debug, Default)]
pub struct SymbolDefinition {
    pub type_: Type,
    pub name: String,
}

/// Represents a `val` static statement in the Aura language.
/// 
/// `val x Int = 10` is represented as:
/// ```rust
/// ValueDefinition {
///     symbol: SymbolDefinition {
///         type_: Type { name: "Int" },
///         name: "x",
///     },
///     value: ValueLiteral::Int(10),
/// }
/// ```
pub struct ValueDefinition {
    pub symbol: SymbolDefinition,
    pub value: ValueLiteral,
}

/// Represents a literal value in the Aura language.
pub enum ValueLiteral {
    Int(i32),
    Float(f32),
    String(String),
    Bool(bool),
}

/// Represents a `fn` static statement in the Aura language.
pub struct FunctionDefinition {
    pub name: String,
    pub args: Vec<SymbolDefinition>,
    pub output: Type,
    pub body: Vec<Statement>,
}

/// Represents a function literal in the Aura language. A closure.
pub struct FunctionLiteral {
    pub args: Vec<SymbolDefinition>,
    pub captures: Vec<SymbolIdentifier>,
    pub output: Expression,
}

pub struct TypeDefinition {
    pub symbol: TypeIdentifier,
    pub type_: Type,
}

pub enum Statement {
    Call {
        symbol: SymbolIdentifier,
        args: Vec<Expression>,
    },
    Bind {
        ident: SymbolIdentifier,
        expr: Expression,
    },
    Return(Expression),
}

pub struct Expression {
    pub value: Value,
    pub type_: Type,
}

pub enum Value {
    Literal(ValueLiteral),
    Call {
        symbol: SymbolIdentifier,
        args: Vec<Expression>,
    },
    Block(Vec<Statement>),
}

#[derive(Debug, Default)]
pub struct Type {
    pub ident: TypeIdentifier,
    pub form: TypeForm,
}

#[derive(Debug, Default)]
pub enum TypeForm {
    Struct(Vec<SymbolDefinition>),
    Enum(Vec<SymbolDefinition>),
    Compound(Vec<Type>),
    Alias(TypeIdentifier),
    Functional(Box<Type>, Box<Type>),
    #[default]
    Void,
}