use super::Compilable;

pub struct Identifier(pub String);

pub struct Symbol {
    pub name: Identifier,
    pub type_: Identifier,
}

impl Compilable for Identifier {
    fn compile(&self) -> String {
        self.0.clone()
    }
}

impl Compilable for Symbol {
    fn compile(&self) -> String {
        format!("{} {}", self.type_.compile(), self.name.compile())
    }
}