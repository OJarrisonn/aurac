use anyhow::Result;
use error::ParseError::*;
use token::Token;

pub mod token;
pub mod error;
pub mod ast;
pub mod parse;

type Lexer<'a> = logos::Lexer<'a, Token>;

/// Take the next token from the lexer and check if it is the expected token
pub fn take_exact(mut lexer: Lexer, token: Token) -> Result<Lexer> {
    match lexer.next() {
        Some(Ok(t)) if t == token => Ok(lexer),
        Some(Ok(token)) => bail!(UnexpectedToken { expected: format!("{:?}", token), token, slice: lexer.slice().into(),span: lexer.span() }),
        Some(Err(_)) => bail!(Unknown),
        None => bail!(UnexpectedEOF),
    }
}