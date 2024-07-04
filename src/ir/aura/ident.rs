
/// A symbol identifier.
/// 
/// Symbols may belong to a type
/// 
/// - `print` a symbol
/// - `String:new` a method in the type `String`
/// - `Vec.max_size` a value in the type `Vec`
#[derive(Debug, Default, Clone)]
pub struct Identifier {
    pub name: String,
}

impl From<String> for Identifier {
    fn from(name: String) -> Self {
        Self { name }
    }
}

impl From<&str> for Identifier {
    fn from(name: &str) -> Self {
        name.to_string().into()
    }
}