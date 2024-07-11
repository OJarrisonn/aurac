use anyhow::{Context, Result};

use crate::lex::{self, ast::{Identifier, TypeExpression}, take_exact, token::Token, Lexer, PeekLexer};


/// Parse a function signature in a function definition
/// 
/// Syntax:
///     ( ValueIdentifier TypeExpression [, ValueIdentifier TypeExpression]* ) -> TypeExpression
/// TODO: Show where the error happened in the source code
/// TODO: Implicit void input type
pub fn parse_function_signature(lexer: Lexer) -> Result<(Lexer, Vec<(Identifier, TypeExpression)>, TypeExpression)> {
    let mut lexer = take_exact(lexer, Token::LParen).context("expected `(` at the start of a function signature")?;

    let mut params = vec![];

    loop {
        if let (peeker, Some(Ok(Token::RParen))) = lexer.peek() {
            lexer = peeker;
            break
        }

        let (l, id) = lex::parse::expr::parse_value_identifier(lexer).context("parsing function parameter identifier")?;
        let (mut l, ty) = lex::parse::types::parse_type(l).context("parsing function parameter type")?;

        params.push((id, ty));

        match l.next() {
            Some(Ok(Token::RParen)) => {
                lexer = l;
                break
            },
            Some(Ok(Token::Comma)) => {
                lexer = l;
                continue
            },
            Some(Ok(token)) => return Err(lex::error::ParseError::UnexpectedToken { 
                token, 
                slice: l.slice().into(), 
                span: l.span(), 
                expected: "`,` or `)`".to_string() 
            }).context("parsing function signature"),
            Some(Err(_)) => return Err(lex::error::ParseError::Unknown).context("parsing function signature"),
            None => return Err(lex::error::ParseError::UnexpectedEOF).context("parsing function signature"),
        }
    }

    // Implicit void return type
    if let (_, Some(Ok(Token::Assign))) = lexer.peek() {
        return Ok((lexer, params, TypeExpression::Identifier(Identifier::Type("Void".to_string()))))
    }

    if let (_, Some(Ok(Token::LBrace))) = lexer.peek() {
        return Ok((lexer, params, TypeExpression::Identifier(Identifier::Type("Void".to_string()))))
    }

    let lexer = take_exact(lexer, Token::Arrow).context("expected `->` in function signature")?;

    let (lexer, ret) = lex::parse::types::parse_type(lexer).context("parsing function return type")?;

    Ok((lexer, params, ret))
}