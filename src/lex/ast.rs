//! The AST is a tree-like structure that represents the parsed code.
//! It is composed of nodes that represent the different parts of the code.

use std::fmt::Display;

/// The root node of a module. Contains all the top-level definitions (`val`, `type`, `fn`, `main` (or `lib`), `import`, `tag`, etc).
#[derive(Debug, Default)]
pub struct Mod {
    /// The literal vals defined in the module.
    pub vals: Vec<Val>,
    /// The types defined in the module.
    pub types: Vec<Type>,
    /// The functions defined in the module.
    pub fns: Vec<Fn>,
    /// The main function of the module.
    pub main: Option<Main>,
}

/// A `val` definition.
/// 
/// Syntax:
/// ```text
/// val <identifier> <type> = <literal>
/// ``
#[derive(Debug)]
pub struct Val {
    /// The identifier of the value. Must be `snake_case`.
    pub symbol: Identifier,
    /// The type of the value. Mandatory.
    pub type_: TypeExpression,
    /// The value of the value. Mandatory.
    pub value: Literal,
}

/// A variant of a `val` definition that may omit the value.
#[derive(Debug)]
pub struct AbstractVal {
    /// The identifier of the value. Must be `snake_case`.
    pub symbol: Identifier,
    /// The type of the value. Mandatory.
    pub type_: TypeExpression,
    /// The value of the value. Mandatory.
    pub value: Option<Literal>,
}

/// A `type` definition.
/// 
/// Syntax:
/// ```text
/// type <identifier> <generics>? = <type_expression> {
///     <assoc_field>*
/// }
/// ```
#[derive(Debug)]
pub struct Type {
    /// The identifier of the type. Must be `PascalCase`.
    pub symbol: Identifier,
    /// The generics of the type. Optional.
    pub generics: Option<Vec<Identifier>>,
    /// The type expression of the type. Mandatory. May be a anonymous or an alias to another type.
    pub type_: TypeExpression,
    /// The associated fields of the type. Optional. They cannot be abstract declarations.
    pub assoc_val: Vec<Val>,
    pub assoc_type: Vec<Type>,
    pub assoc_fn: Vec<Fn>,
}

/// A variant of a `type` definition that may omit the type expression.
#[derive(Debug)]
pub struct AbstractType {
    /// The identifier of the type. Must be `PascalCase`.
    pub symbol: Identifier,
    /// The type expression of the type. Mandatory. May be a anonymous or an alias to another type.
    pub type_: Option<TypeExpression>,
}


/// A `tag` definition.
/// 
/// Syntax:
/// ```text
/// tag <identifier> <generics>? <supertag> {
///     <assoc_field>*
/// }
/// ```
#[derive(Debug)]
pub struct Tag {
    /// The identifier of the tag. Must be `#kebab-case`.
    pub symbol: Identifier,
    /// The generics of the tag. Optional.
    pub generics: Option<Vec<GenericTypeDeclaration>>,
    /// The super tag of the tag. Optional.
    pub supertag: Option<TagExpression>,
    /// The associated fields of the type. Optional. They can be abstract declarations.
    pub assoc_val: Vec<AbstractVal>,
    pub assoc_type: Vec<AbstractType>,
    pub assoc_fn: Vec<AbstractFn>,
}

#[derive(Debug)]
pub struct TagImpl {
    pub type_: TypeExpression,
    pub tag: TagExpression,
    pub assoc_val: Vec<Val>,
    pub assoc_type: Vec<Type>,
    pub assoc_fn: Vec<Fn>,
}

/// A `fn` definition.
/// 
/// Syntax:
/// ```text
/// fn <identifier> (<generics>?; <input>*) -> <output>? [= <expression> | { <body> }]
/// fn <identifier> (<generics>?) -> <output>? [= <expression> | { <body> }]
/// fn <identifier> (<input>*) -> <output> [= <expression> | { <body> }]
/// ```
#[derive(Debug)]
pub struct Fn {
    /// The identifier of the function. Must be `snake_case`.
    pub symbol: Identifier,
    /// The generics of the function. Optional.
    pub generics: Option<Vec<GenericTypeDeclaration>>,
    /// The input parameters of the function. Mandatory.
    pub input: Vec<(Identifier, TypeExpression)>,
    /// The output type of the function. Mandatory.
    pub output: TypeExpression,
    /// The body of the function. May be a expression or a block.
    pub body: Expression,
}

/// A variant of a `fn` definition that may omit the body.
#[derive(Debug)]
pub struct AbstractFn {
    pub symbol: Identifier,
    pub generics: Option<Vec<GenericTypeDeclaration>>,
    pub input: Vec<(Identifier, TypeExpression)>,
    pub output: TypeExpression,
    pub body: Option<Expression>,
}

/// The `main` function of a module.
/// 
/// Syntax:
/// ```text
/// main (<input>*)? -> <output>? [= <expression> | { <body> }]
/// ```
#[derive(Debug)]
pub struct Main {
    pub input: Vec<(Identifier, TypeExpression)>,
    pub output: TypeExpression,
    pub body: Expression,
}

/// An expression with a known value.
#[derive(Debug)]
pub struct Expression {
    pub expression: UntypedExpression,
    pub type_: Option<TypeExpression>,
}

/// An expression without a known value. Produces values.
#[derive(Debug)]
pub enum UntypedExpression {
    /// A literal value that's is known at compile time and completly immutable.
    Literal(Literal),
    /// An identifier of a value
    Identifier(Identifier),
    /// The expression for a compound type, produces a compound value
    Compound(Vec<Expression>),
    /// The expression for a struct type, produces a struct value
    Struct(Vec<(Option<Identifier>, Expression)>),
    /// The expression for a function declaration (a closure)
    Fn(Vec<Identifier>, Box<Expression>),
    /// The expression for a list of expressions
    List(Vec<Expression>),
    /// The expression for a call to a function
    Call(Call),
    /// The expression for a field access of another expression
    ExprField(Box<Expression>, Identifier),
    /// The expression for a associated member access of an expression
    ExprAssoc(Box<Expression>, Identifier),
    /// The expression to build a variant of an expression that is an enum
    ExprEnumBuild(Box<Expression>, Call),
    /// The expression for a call of a method in an expression
    ExprAssocCall(Box<Expression>, Call),
    /// The expression for a field access of a type
    TypeField(TypeExpression, Identifier),
    /// The expression for a associated member access of a type
    TypeAssoc(TypeExpression, Identifier),
    /// The expression to build a variant of an enum
    TypeEnumBuild(TypeExpression, Call),
    /// The expression for a call of a method in a type
    TypeAssocCall(TypeExpression, Call),
    /// The expression for a associated member access of a tag
    TagAssoc(TagExpression, Identifier),
    /// The expression for a call of a method in a tag
    TagAssocCall(TagExpression, Call),
    /// The expression for a block of expressions
    Block(Vec<Expression>),
    /// The expression for an infix binary operation
    BinOp(Box<Expression>, BinOp, Box<Expression>),
    /// The expression for an unary operation
    UnOp(UnOp, Box<Expression>),
    /// The expression for a type operation over an expression
    TypeOp(Box<Expression>, TypeOp, TypeExpression),
    /// Don't Care, used for currying expressions into functions
    DontCare,
}

/// A call to a function.
/// 
/// This function maybe: a function, an assoc function from a type, an assoc function from a tag, a variant of an enum.
#[derive(Debug)]
pub struct Call {
    /// The symbol being called (will be bound depending on the calling context)
    pub symbol: Identifier,
    /// The "concrete" types of the generics of the symbol
    pub generics: Option<Vec<TypeExpression>>,
    /// The arguments of the call
    pub args: Vec<Expression>,
}

/// A binary operation.
#[derive(Debug)]
pub enum BinOp {
    /// +
    Add,
    /// -
    Sub,
    /// *
    Mul,
    /// /
    Div,
    /// %
    Mod,
    /// **
    Pow,
    /// &&
    And,
    /// ||
    Or,
    /// ==
    Eq,
    /// !=
    Neq,
    /// <
    Lt,
    /// >
    Gt,
    /// <=
    Le,
    /// >=
    Ge,
    /// >>
    BitShl,
    /// <<
    BitShr,
    /// |>
    Pipe,
    /// ??
    Unwrap,
    /// ?=
    Default,
    /// ...
    Spread,
    /// ++
    Concat,
    /// ..
    Range,
    /// ..=
    RangeInclusive,
    /// ::
    Join,
    /// \\
    Split,
}

/// An unary operation.
#[derive(Debug)]
pub enum UnOp {
    /// -
    Neg,
    /// !
    Not,
}

/// A type operation.
#[derive(Debug)]
pub enum TypeOp {
    /// $>
    Cast,
    /// $$
    OfType,
}

/// A type expression.
#[derive(Debug)]
pub enum TypeExpression {
    /// An identifier of a defined type (with `type` or `tag` or generic declarations)
    Identifier(Identifier),
    /// A function type
    Fn(Vec<GenericTypeDeclaration>, StructTypeExpression, Box<TypeExpression>),
    /// A compound type, a list of types
    Compound(Vec<TypeExpression>),
    /// A struct type, a list of fields with types and maybe a default value
    Struct(StructTypeExpression),
    /// A union type, a set of types
    Union(Vec<TypeExpression>),
    /// An enum type, a list of variants
    Enum(Vec<(Identifier, TypeExpression)>),
    /// A type that uses generics
    Parametrized(Identifier, Vec<TypeExpression>),
    /// A type built from a tag (a set of types, similar to an union)
    Tag(TagExpression),
    /// A type defined in a `type` in side another `type` or `tag`
    AssocType(Box<TypeExpression>, Identifier),
}

#[derive(Debug)]
pub struct StructTypeExpression(Vec<(Identifier, Box<TypeExpression>, Option<Expression>)>);

/// A tag expression.
#[derive(Debug)]
pub enum TagExpression {
    /// A simple tag
    Identifier(Identifier),
    /// A compound tag, a list of tags (the intersection of types that are tagged with all the tags in the list)
    Compound(Vec<TagExpression>),
    /// A tag that uses generics
    Parametrized(Identifier, Vec<TypeExpression>),
}

/// A generic type declaration.
/// 
/// Used in:
/// - `type` definitions by the name of the type
/// - `tag` definitions by the name of the tag
/// - `fn` definitions inside the `( )` before the `;`
#[derive(Debug)]
pub struct GenericTypeDeclaration {
    /// The identifier of the generic type. Must be `snake_case`.
    pub symbol: Identifier,
    /// The bound of the generic type. Optional.
    pub bound: Option<TagExpression>,
}

/// An identifier. The name of a value, type, tag, etc.
#[derive(Debug)]
pub enum Identifier {
    /// `snake_case` identifier for a value
    Value(String),
    /// `PascalCase` identifier for a type
    Type(String),
    /// `$PascalCase` identifier for a generic
    GenericType(String),
    /// `#kebab-case` or `kebab-case`(inside a compound tag notation) identifier for a tag
    Tag(String),
}

/// A literal value.
#[derive(Debug)]
pub enum Literal {
    /// An integer value
    Integer(i64),
    /// A float value
    Float(f64),
    /// A string value
    String(String),
    /// A char value
    Char(char),
    /// A list value
    List(Vec<Literal>),
    /// A compound value
    Compound(Vec<Literal>),
    /// A struct value
    Struct(Vec<(Option<Identifier>, Literal)>),
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Identifier::Value(s) | Identifier::Type(s) | Identifier::Tag(s) | Identifier::GenericType(s) => write!(f, "{}", s),
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

impl From<char> for Literal {
    fn from(c: char) -> Self {
        Literal::Char(c)
    }
}

impl<T: Into<Literal>> From<Vec<T>> for Literal {
    fn from(l: Vec<T>) -> Self {
        Literal::List(l.into_iter().map(|x| x.into()).collect())
    }
}

impl<T: Into<Literal>> From<Vec<(Option<Identifier>, T)>> for Literal {
    fn from(l: Vec<(Option<Identifier>, T)>) -> Self {
        Literal::Struct(l.into_iter().map(|(i, x)| (i.map(|i| i.into()), x.into())).collect())
    }
}