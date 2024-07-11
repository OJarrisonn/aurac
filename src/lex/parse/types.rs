use anyhow::{Context, Result};

use crate::lex::{ast::{Identifier, TypeExpression}, token::Token, Lexer, error::ParseError::*, PeekLexer};

/// Parses a type expression
/// 
/// Syntax:
///     TypeIdentifier
///     ( TypeExpression [, TypeExpression]* )
pub fn parse_type(mut lexer: Lexer) -> Result<(Lexer, TypeExpression)> {
    let init = lexer.span().end;

    match lexer.next() {
        Some(Ok(Token::TypeIdentifier(t))) => Ok((lexer, TypeExpression::Identifier(Identifier::Type(t)))),
        Some(Ok(Token::LParen)) => {
            let src = lexer.source()[init..lexer.span().end].trim();
            parse_compound_type(lexer)
            .map(|(l, te)| (l, TypeExpression::Compound(te)))
            .with_context(|| format!("parsing compound type expression at `{}`", src))
        },
        Some(Ok(token)) => return Err(UnexpectedToken { 
            token, 
            slice: lexer.slice().into(), 
            span: lexer.span(), 
            expected: "type expression".to_string() 
        }).with_context(|| format!("parsing type expression `{}`", lexer.source()[init..lexer.span().end].trim())),
        Some(Err(_)) => bail!(Unknown),
        None => bail!(UnexpectedEOF),
    }
}

pub fn parse_compound_type(mut lexer: Lexer) -> Result<(Lexer, Vec<TypeExpression>)> {
    let mut compound = vec![];
    let init = lexer.span().start;            
    
    loop {
        // Check if the next token is a right parenthesis
        // If so, return the compound type expression
        if let (peeker, Some(Ok(Token::RParen))) = lexer.peek() {
            return Ok((peeker, compound))
        }

        let (l, te) = {
            let src = lexer.source()[init..lexer.span().end].trim();
            parse_type(lexer).with_context(|| format!("parsing compound type expression `{}`", src))?
        };

        lexer = l;
        compound.push(te);

        match lexer.next() {
            // End of compound type expession
            Some(Ok(Token::RParen)) => return Ok((lexer, compound)), 
            // Next field in compound type expression
            Some(Ok(Token::Comma)) => continue,
            // Unexpected token
            Some(Ok(token)) => return Err(UnexpectedToken { 
                token, 
                slice: lexer.slice().into(), 
                span: lexer.span(), 
                expected: "`,` or `)`".to_string() 
            }).with_context(|| format!("parsing compound type expression `{}`", lexer.source()[init..lexer.span().end].trim())),
            Some(Err(_)) => bail!(Unknown),
            None => bail!(UnexpectedEOF),
        }
    }
}