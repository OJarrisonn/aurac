use super::{identifier::Identifier, Compilable, Statement};

pub struct Expr {
    pub value: Value,
    pub type_: Identifier,
}

pub enum Value {
    Literal(Literal),
    Call(Call),
    Atom(Atom),
    Identifier(Identifier),
    Scope(Scope),
}

/// The IR object for a primitive literal value.
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
}

pub struct Scope(pub Vec<Statement>);

pub struct Call {
    pub symbol: String,
    pub args: Vec<Value>,
}

pub struct Atom(String);

impl Compilable for Expr {
    fn compile(&self) -> String {
        self.value.compile()
    }
}

impl Compilable for Value {
    fn compile(&self) -> String {
        match self {
            Value::Literal(literal) => literal.compile(),
            Value::Call(call) => call.compile(),
            Value::Atom(atom) => atom.compile(),
            Value::Identifier(identifier) => identifier.compile(),
            Value::Scope(scope) => scope.compile(),
        }
    }
}

impl Compilable for Literal {
    fn compile(&self) -> String {
        match self {
            Literal::Int(i) => i.to_string(),
            Literal::Float(f) => f.to_string(),
            Literal::String(s) => format!("\"{}\"", s),
            Literal::Bool(b) => b.to_string(),
        }
    }
}

impl Compilable for Scope {
    fn compile(&self) -> String {
        let exprs = self.0.iter().map(|expr| expr.compile()).collect::<Vec<String>>().join(";\n\t");
        format!("{{\n\t{};\n}}", exprs)
    }
}

impl Compilable for Call {
    fn compile(&self) -> String {
        let args = self.args.iter().map(|arg| arg.compile()).collect::<Vec<String>>().join(", ");
        format!("{}({})", self.symbol, args)
    }
}

impl Atom {
    fn from_identifier(id: &str) -> Self {
        Self(format!("__Atom_{}", id.to_uppercase().replace("-", "_")))
    }

    fn into_identifier(&self) -> String {
        self.0.trim_start_matches("__Atom_").to_lowercase().replace("_", "-")
    }
}

impl From<&str> for Atom {
    fn from(id: &str) -> Self {
        Atom::from_identifier(id)
    }
}

impl Into<String> for Atom {
    fn into(self) -> String {
        self.into_identifier()
    }
}

impl Compilable for Atom {
    fn compile(&self) -> String {
        self.0.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_literal_compile() {
        assert_eq!(Literal::Int(42).compile(), "42");
        assert_eq!(Literal::Float(3.14).compile(), "3.14");
        assert_eq!(Literal::String("hello".into()).compile(), "\"hello\"");
        assert_eq!(Literal::Bool(true).compile(), "true");
        assert_eq!(Literal::Bool(false).compile(), "false");
    }
}