use lex::{parse_val, token::Token};
use logos::Logos;

#[macro_use]
extern crate anyhow;

//mod ir;
mod lex;


fn main() -> anyhow::Result<()> {
    let src = include_str!("../examples/hello_world.aura");
    let lexer = Token::lexer(src);
    let _ = dbg!(parse_val(lexer)?);

    Ok(())
}
