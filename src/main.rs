use lex::token::Token;
use logos::Logos;

mod ir;
mod lex;


fn main() {
    let src = include_str!("../examples/hello_world.aura");
    let lexer = Token::lexer(src);
    let tokens = lexer.take_while(|l| l.is_ok()).map(Result::unwrap).collect::<Vec<_>>();
    println!("{:?}", tokens);
}
