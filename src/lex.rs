use anyhow::Result;
use error::ParseError::*;
use token::Token;

pub mod token;
pub mod error;
pub mod ast;
pub mod parse;

type Lexer<'a> = logos::Lexer<'a, Token>;

pub trait PeekLexer<'a> {
    /// Peek at the next token in the lexer. This provides a way to look at the next token without consuming it.
    /// 
    /// Returns a new lexer where the token is consumed and the next token if it exists
    fn peek(&self) -> (Lexer<'a>, Option<Result<Token>>);
}

impl<'lexer> PeekLexer<'lexer> for Lexer<'lexer> {
    fn peek(&self) -> (Lexer<'lexer>, Option<Result<Token>>) {
        let mut peeker = self.clone();
        let next = peeker.next().map(|r| r.map_err(|_| Unknown.into())); 
        
        (peeker, next)
    }
}

/// Take the next token from the lexer and check if it is the expected token
pub fn take_exact(mut lexer: Lexer, token: Token) -> Result<Lexer> {
    match lexer.next() {
        Some(Ok(t)) if t == token => Ok(lexer),
        Some(Ok(token)) => bail!(UnexpectedToken { expected: format!("{:?}", token), token, slice: lexer.slice().into(),span: lexer.span() }),
        Some(Err(_)) => bail!(Unknown),
        None => bail!(UnexpectedEOF),
    }
}