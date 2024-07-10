use anyhow::Result;

use crate::lex::{ast::{Identifier, Literal}, error::ParseError::*, token::Token::*, Lexer};

pub fn parse_identifier(mut lexer: Lexer) -> Result<(Lexer, Identifier)> {
    match lexer.next() {
        Some(Ok(ValueIdentifier(s))) => Ok((lexer, Identifier::Value(s))),
        Some(Ok(TypeIdentifier(s))) => Ok((lexer, Identifier::Type(s))),
        Some(Ok(TagIdentifier(s))) => Ok((lexer, Identifier::Tag(s))),
        Some(Ok(token)) => bail!(UnexpectedToken { 
            token, 
            slice: lexer.slice().into(), 
            span: lexer.span(), 
            expected: "identifier".to_string() 
        }),
        Some(Err(_)) => bail!(Unknown),
        None => bail!(UnexpectedEOF),
    }
}

pub fn parse_value_identifier(mut lexer: Lexer) -> Result<(Lexer, Identifier)> {
    match lexer.next() {
        Some(Ok(ValueIdentifier(s))) => Ok((lexer, Identifier::Value(s))),
        Some(Ok(token)) => bail!(UnexpectedToken { 
            token, 
            slice: lexer.slice().into(), 
            span: lexer.span(), 
            expected: "value identifier".to_string() 
        }),
        Some(Err(_)) => bail!(Unknown),
        None => bail!(UnexpectedEOF),
    }
}

pub fn parse_type_identifier(mut lexer: Lexer) -> Result<(Lexer, Identifier)> {
    match lexer.next() {
        Some(Ok(TypeIdentifier(s))) => Ok((lexer, Identifier::Type(s))),
        Some(Ok(token)) => bail!(UnexpectedToken { 
            token, 
            slice: lexer.slice().into(), 
            span: lexer.span(), 
            expected: "type identifier".to_string() 
        }),
        Some(Err(_)) => bail!(Unknown),
        None => bail!(UnexpectedEOF),
    }
}

pub fn parse_tag_identifier(mut lexer: Lexer) -> Result<(Lexer, Identifier)> {
    match lexer.next() {
        Some(Ok(TagIdentifier(s))) => Ok((lexer, Identifier::Tag(s))),
        Some(Ok(token)) => bail!(UnexpectedToken { 
            token, 
            slice: lexer.slice().into(), 
            span: lexer.span(), 
            expected: "tag identifier".to_string() 
        }),
        Some(Err(_)) => bail!(Unknown),
        None => bail!(UnexpectedEOF),
    }
}

pub fn parse_literal(mut lexer: Lexer) -> Result<(Lexer, Literal)> {
    match lexer.next() {
        Some(Ok(IntegerLiteral(i))) => Ok((lexer, Literal::Integer(i))),
        Some(Ok(FloatLiteral(f))) => Ok((lexer, Literal::Float(f))),
        Some(Ok(StringLiteral(s))) => Ok((lexer, Literal::String(s))),
        Some(Ok(token)) => bail!(UnexpectedToken { 
            token, 
            slice: lexer.slice().into(), 
            span: lexer.span(), 
            expected: "literal".to_string() 
        }),
        Some(Err(_)) => bail!(Unknown),
        None => bail!(UnexpectedEOF),
    }
}