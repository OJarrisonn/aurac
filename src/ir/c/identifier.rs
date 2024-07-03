use super::Compilable;

pub struct Identifier(pub String);

impl Compilable for Identifier {
    fn compile(&self) -> String {
        self.0.clone()
    }
}