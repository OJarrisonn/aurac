use lex::{parse::defn::parse_mod, token::Token};
use logos::Logos;

#[macro_use]
extern crate anyhow;

//mod ir;
pub mod lex;
pub mod utils;

fn main() -> anyhow::Result<()> {
    let src = include_str!("../examples/hello_world.aura");
    let lexer = Token::lexer(src);
    let _ = dbg!(parse_mod(lexer)?);

    Ok(())
}
