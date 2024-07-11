use anyhow::Result;
use error::ParseError::*;
use parse::ParseResult;
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
pub fn take_exact(mut lexer: Lexer, token: Token) -> ParseResult<()> {
    match lexer.next() {
        Some(Ok(t)) if t == token => ok!(lexer),
        Some(Ok(token)) => err!(lexer, UnexpectedToken { expected: format!("{:?}", token), token, slice: lexer.slice().into(),span: lexer.span() }),
        Some(Err(_)) => err!(lexer, Unknown),
        None => err!(lexer, UnexpectedEOF),
    }
}

pub fn lexer_read_source<'source>(lexer: &Lexer<'source>, init: usize) -> &'source str {
    lexer.source()[init..lexer.span().end].trim()
}