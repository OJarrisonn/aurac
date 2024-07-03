//! Definitions for the Aura Intermediate Representation (IR).

pub mod c;

/// A trait implemented by IR objects to compile to a given target language.
pub trait Compilable {
    // TODO: The target language as a type parameter?

    /// Compile the IR object to a target language.
    /// returns: The compiled code as a string.
    fn compile(&self) -> String;
}