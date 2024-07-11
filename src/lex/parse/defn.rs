use anyhow::{Context, Result};
use fn_::parse_function_signature;

use crate::lex::{ast::{Expression, Fn, Identifier, Main, Mod, Type, TypeExpression, Val}, error::ParseError::*, take_exact, token::Token, Lexer, PeekLexer};

use super::{expr::{parse_expression, parse_literal, parse_type_identifier, parse_value_identifier}, types::parse_type};

pub mod fn_;

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
            Some(Ok(Token::Fn)) => {
                let (l, f) = parse_fn_defn(lexer).context("parsing `fn` definition in module")?;
                lexer = l;
                module.fns.push(f);
            },
            Some(Ok(Token::Main)) => {
                let (l, m) = parse_main_defn(lexer).context("parsing `main` definition in module")?;
                lexer = l;
                module.main = Some(m);
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

pub fn parse_fn_defn(lexer: Lexer) -> Result<(Lexer, Fn)> {
    let lexer = take_exact(lexer, Token::Fn).context("expected `fn` definition")?;

    let (lexer, symbol) = parse_value_identifier(lexer).context("parsing the symbol of a `fn` definition")?;

    let (mut lexer, input, output) = parse_function_signature(lexer).context("parsing function signature in `fn` definition")?;

    // Foresee the next token if it's a `=` or `{`
    let allow_non_block = match lexer.peek() {
        (peeker, Some(Ok(Token::Assign))) => {
            lexer = peeker;
            true
        },
        (_, Some(Ok(Token::LBrace))) => false,
        (_, Some(Ok(token))) => return Err(UnexpectedToken { 
            token, 
            slice: lexer.slice().into(), 
            span: lexer.span(), 
            expected: "`{` or `=`".to_string() 
        }).context("parsing `fn` definition"),
        (_, Some(Err(_))) => return Err(Unknown).context("parsing `fn` definition"),
        (_, None) => return Err(UnexpectedEOF).context("parsing `fn` definition"),
    };

    let (lexer, body) = parse_expression(lexer).context("parsing expression in `fn` definition")?;

    // If allow_non_block is true then the body is any expression
    // If allow_non_block is false then the body is a block expression
    if !allow_non_block {
        match body {
            Expression::Block(_) => (),
            _ => return Err(UnexpectedToken { 
                token: Token::LBrace, 
                slice: lexer.slice().into(), 
                span: lexer.span(), 
                expected: "block expression or `=`".to_string() 
            }).context("parsing `fn` definition"),
        }
    }

    Ok((lexer, Fn { symbol, input, output, body }))
}

pub fn parse_main_defn(lexer: Lexer) -> Result<(Lexer, Main)> {
    let lexer = take_exact(lexer, Token::Main).context("expected `main` definition")?;

    // Check if a function signature is present
    let (_, future_token) = lexer.peek();

    // `(`, `->`, `=` or `{`
    let (lexer, input, output) = match future_token {
        Some(Ok(Token::LParen)) | Some(Ok(Token::Arrow)) => parse_function_signature(lexer).context("parsing function signature in `main` definition")?,
        Some(Ok(Token::Assign)) | Some(Ok(Token::LBrace)) => (lexer, vec![], TypeExpression::Identifier(Identifier::Type("Void".to_string()))),
        Some(Ok(token)) => return Err(UnexpectedToken { 
            token, 
            slice: lexer.slice().into(), 
            span: lexer.span(), 
            expected: "`(` or `=`".to_string() 
        }).context("parsing `main` definition"),
        Some(Err(_)) => return Err(Unknown).context("parsing `main` definition"),
        None => return Err(UnexpectedEOF).context("parsing `main` definition"),
    };

    let (lexer, allow_non_block) = if let (lexer, Some(Ok(Token::Assign))) = lexer.peek() { (lexer, true) } else { (lexer, false) };

    let (lexer, body) = parse_expression(lexer).context("parsing expression in `main` definition")?;

    if !allow_non_block {
        match body {
            Expression::Block(_) => (),
            _ => return Err(UnexpectedToken { 
                token: Token::LBrace, 
                slice: lexer.slice().into(), 
                span: lexer.span(), 
                expected: "block expression or `=`".to_string() 
            }).context("parsing `main` definition"),
        }
    }

    Ok((lexer, Main { input, output, body }))
}