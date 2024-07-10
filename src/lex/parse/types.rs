use anyhow::{Context, Result};

use crate::lex::{ast::{Identifier, TypeExpression}, token::Token, Lexer, error::ParseError::*};

pub fn parse_type(mut lexer: Lexer) -> Result<(Lexer, TypeExpression)> {
    let init = lexer.span().end;

    match lexer.next() {
        Some(Ok(Token::TypeIdentifier(t))) => Ok((lexer, TypeExpression::Identifier(Identifier::Type(t)))),
        Some(Ok(Token::LParen)) => {
            let mut compound = vec![];
            let init = lexer.span().start;            
            
            loop {
                let (l, te) = {
                    let src = lexer.source()[init..lexer.span().end].trim();
                    parse_type(lexer).with_context(|| format!("parsing compound type expression `{}`", src))
                }?;

                lexer = l;
                compound.push(te);

                match lexer.next() {
                    Some(Ok(Token::RParen)) => return Ok((lexer, TypeExpression::Compound(compound))),
                    Some(Ok(Token::Comma)) => continue,
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