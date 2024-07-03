//! Defines the IR object for a program.

use expr::{Expr, Literal, Scope, Value};
use identifier::Symbol;

use self::{expr::Atom, identifier::Identifier};

use super::Compilable;

pub mod expr;
pub mod identifier;

/// The IR object for a program. It represents a correct Aura program (syntax and semantics).
/// 
/// The program should contain enough information to produce a correct C program.
/// 
/// No further processing is needed to produce a C program from this object. Just produce string output.
pub struct CIR {
    /// The list of C header files to include in the compilation.
    pub includes: Vec<String>,

    /// Atoms are defined globally in the program.
    /// A total of 2^64 atoms can be defined in a program.
    pub atoms: Vec<Atom>,

    /// The static functions and values defined in the program.
    pub statics: Vec<StaticBind>,

    /// The main function of the program.
    pub main: StaticBind,
}

pub enum Statement {
    Expr(Expr),
    Bind{ident: Identifier, expr: Expr},
    Return(Value),
}

pub enum StaticBind {
    Value { ident: Identifier, value: Literal },
    Function { header: FunctionHeader, body: Scope },
}

pub struct FunctionHeader {
    pub ident: Identifier,
    pub args: Vec<Symbol>,
    pub output: Identifier,
}

impl StaticBind {
    pub fn is_value(&self) -> bool {
        match self {
            StaticBind::Value{..} => true,
            _ => false,
        }
    }

    pub fn is_function(&self) -> bool {
        match self {
            StaticBind::Function{..} => true,
            _ => false,
        }
    }

    pub fn as_value(&self) -> &Literal {
        match self {
            StaticBind::Value{value, ..} => value,
            _ => panic!("Not a value"),
        }
    }

    pub fn as_function(&self) -> (&FunctionHeader, &Scope) {
        match self {
            StaticBind::Function{header, body} => (header, body),
            _ => panic!("Not a function"),
        }
    }
}

impl Compilable for CIR {
    fn compile(&self) -> String {
        let mut code = String::new();

        code.push_str(include_str!("c/core.h"));

        code.push_str("\n// Section includes\n");
        for include in &self.includes {
            code.push_str(&format!("#include \"{}\"\n", include));
        }

        code.push_str("\n// Section atoms\n");
        for (i, atom) in self.atoms.iter().enumerate() {
            code.push_str(&format!("#define {} {}\n", atom.compile(), i));
        }

        code.push_str("\n// Section statics\n");
        code.push_str("// Signatures\n");
        self.statics.iter().filter(|bind| bind.is_function()).for_each(|function| {
            let (header, _) = function.as_function();
            code.push_str(&header.compile());
            code.push_str(";\n");
        });

        code.push_str("\n// Definitions\n");
        for static_ in &self.statics {
            code.push_str(&static_.compile());
            code.push_str("\n"); 
        }

        code.push_str("\n// Section main\n");
        code.push_str(&self.main.compile());
        code
    }

}

impl Compilable for Statement {
    fn compile(&self) -> String {
        match self {
            Statement::Expr(expr) => expr.compile(),
            Statement::Bind{ident, expr} => {
                format!("{} {} = {}",
                    expr.type_.compile(),
                    ident.compile(),
                    expr.compile()
                )
            },
            Statement::Return(value) => {
                format!("return {}", value.compile())
            }
        }
    }
}

impl Compilable for StaticBind {
    fn compile(&self) -> String {
        match self {
            StaticBind::Value{ident, value} => {
                format!("const {} {} = {};",
                    match value {
                        Literal::Int(_) => "Int",
                        Literal::Float(_) => "Float",
                        Literal::String(_) => "String",
                        Literal::Bool(_) => "Bool",
                    },
                    ident.compile(),
                    value.compile()
                )
            },
            StaticBind::Function{header, body} => {
                format!("{} {}",
                    header.compile(),
                    body.compile()
                )
            }
        }
    }
}

impl Compilable for FunctionHeader {
    fn compile(&self) -> String {
        let args = self.args.iter().map(|arg| arg.compile()).collect::<Vec<String>>().join(", ");
        format!("{} {}({})",
            self.output.compile(),
            self.ident.compile(),
            args
        )
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_program_compile() {

    }
}