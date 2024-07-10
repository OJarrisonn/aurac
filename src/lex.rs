use anyhow::{Context, Result};
use error::ParseError::*;
use parse::{Identifier, Val};
use token::Token;

pub mod token;
pub mod error;
pub mod parse;

type Lexer<'a> = logos::Lexer<'a, Token>;

pub fn parse_identifier(mut lexer: Lexer) -> Result<(Lexer, Identifier)> {
    match lexer.next() {
        Some(Ok(Token::ValueIdentifier(s))) => Ok((lexer, Identifier::Value(s))),
        Some(Ok(Token::TypeIdentifier(s))) => Ok((lexer, Identifier::Type(s))),
        Some(Ok(Token::TagIdentifier(s))) => Ok((lexer, Identifier::Tag(s))),
        Some(Ok(token)) => bail!(UnexpectedToken { token, slice: lexer.slice().into(), span: lexer.span(), expected: "identifier".to_string() }),
        Some(Err(_)) => bail!(Unknown),
        None => bail!(UnexpectedEOF),
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

/// Parse a `val` declaration
/// 
/// Syntax: 
/// ```ignore
/// val symbol Type = expression;
/// ```
pub fn parse_val(lexer: Lexer) -> Result<(Lexer, Val)> {
    // `val`
    let lexer = take_exact(lexer, Token::Val)?; 

    // `symbol`
    let (lexer, symbol) = match parse_identifier(lexer) {
        Ok((l, i@Identifier::Value(_))) => (l, i),
        Ok((l, i)) => bail!(UnexpectedIdentifier { identifier: i.to_string(), span: l.span(), expected: "value identifier".to_string()}),
        Err(e) => return Err(e).context("parsing symbol in `val` definition"),
    };

    // `Type`
    let (lexer, type_) = match parse_identifier(lexer) {
        Ok((l, i@Identifier::Type(_))) => (l, i),
        Ok((l, i)) => bail!(UnexpectedIdentifier { identifier: i.to_string(), span: l.span(), expected: "value identifier".to_string()}),
        Err(e) => return Err(e).context("parsing type in `val` definition"),
    };

    // `=`
    let mut lexer = take_exact(lexer, Token::Assign).context("before literal expression in `val` definition")?;

    // `expression`
    let value = match lexer.next() {
        Some(Ok(Token::IntegerLiteral(i))) => i,
        Some(Ok(token)) => bail!(UnexpectedToken { token, slice: lexer.slice().into(), span: lexer.span(), expected: "integer literal".to_string() }),
        Some(Err(_)) => bail!(Unknown),
        None => bail!(UnexpectedEOF),
    };

    Ok((lexer, Val { symbol, type_, value }))
}