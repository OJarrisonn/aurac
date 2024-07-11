use anyhow::{Context, Result};

use crate::lex::{ast::{Mod, Type, Val}, error::ParseError::*, take_exact, token::Token, Lexer, PeekLexer};

use super::{expr::{parse_literal, parse_type_identifier, parse_value_identifier}, types::parse_type};

/// Parse a module
/// A module is an Aura source code file with global definitions for the module
pub fn parse_mod(mut lexer: Lexer) -> Result<Mod> {
    let mut module = Mod::default();
    
    loop {
        let (_, future_token) = lexer.peek();

        match future_token {
            Some(Ok(Token::Val)) => {
                let (l, v) = parse_val_defn(lexer).context("parsing `val` definition in module")?;
                lexer = l;
                module.vals.push(v);
            },
            Some(Ok(Token::Type)) => {
                let (l, t) = parse_type_defn(lexer).context("parsing `type` definition in module")?;
                lexer = l;
                module.types.push(t);
            },
            Some(Ok(token)) => return Err(UnexpectedToken { 
                token, 
                slice: lexer.slice().into(), 
                span: lexer.span(), 
                expected: "`val` definition".to_string() 
            }).context("parsing module"),
            Some(Err(_)) => return Err(Unknown).context("parsing module"),
            None => break,
        };
    }

    Ok(module)
}

/// Parse a `val` declaration
/// 
/// Syntax: 
/// ```ignore
/// val symbol Type = expression;
/// ```
pub fn parse_val_defn(lexer: Lexer) -> Result<(Lexer, Val)> {
    // `val`
    let lexer = take_exact(lexer, Token::Val).context("expected `val` definition")?; 

    // `symbol`
    let (lexer, symbol) = parse_value_identifier(lexer).context("parsing the symbol of a `val` definition")?;

    // `Type`
    let (lexer, type_) = parse_type(lexer).context("parsing type in `val` definition")?;

    // `=`
    let lexer = take_exact(lexer, Token::Assign).context("parsing `=` in `val` definition")?;

    // `expression`
    let (lexer, value) = parse_literal(lexer).context("parsing literal expression in `val` definition")?;

    Ok((lexer, Val { symbol, type_, value }))
}

pub fn parse_type_defn(lexer: Lexer) -> Result<(Lexer, Type)> {
    let lexer = take_exact(lexer, Token::Type).context("expected `type` definition")?;
    let (lexer, symbol) = parse_type_identifier(lexer).context("parsing the symbol of a `type` definition")?;
    let lexer = take_exact(lexer, Token::Assign).context("parsing `=` in `type` definition")?;
    let (lexer, type_) = parse_type(lexer).context("parsing type in `type` definition")?;

    Ok((lexer, Type { symbol, type_ }))
}