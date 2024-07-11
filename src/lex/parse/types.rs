use crate::lex::{ast::{Identifier, TypeExpression}, error::ParseError::*, lexer_read_source, token::Token, Lexer, PeekLexer};

use super::ParseResult;

/// Parses a type expression
/// 
/// Syntax:
///     TypeIdentifier
///     ( TypeExpression [, TypeExpression]* )
pub fn parse_type(mut lexer: Lexer) -> ParseResult<TypeExpression> {
    let init = lexer.span().end;

    match lexer.next() {
        Some(Ok(Token::TypeIdentifier(t))) => ok!(lexer, TypeExpression::Identifier(Identifier::Type(t))),
        Some(Ok(Token::LParen)) => parse_compound_type(lexer).map(|te| TypeExpression::Compound(te)).with_context(|lex| format!("parsing compound type expression in `{}`", lexer_read_source(lex, init))),
        Some(Ok(token)) => err!(lexer, UnexpectedToken { 
            token, 
            slice: lexer.slice().into(), 
            span: lexer.span(), 
            expected: "type expression".to_string() 
        }).with_context(|lex| format!("parsing type expression `{}`", lexer_read_source(lex, init))),
        Some(Err(_)) => err!(lexer, Unknown),
        None => err!(lexer, UnexpectedEOF),
    }
}

pub fn parse_compound_type(mut lexer: Lexer) -> ParseResult<Vec<TypeExpression>> {
    let mut compound = vec![];
    let init = lexer.span().start;            
    
    loop {
        // Check if the next token is a right parenthesis
        // If so, return the compound type expression
        if let (peeker, Some(Ok(Token::RParen))) = lexer.peek() {
            return ok!(peeker, compound)
        }

        let (l, te) = withctx!(parse_type(lexer), "parsing compound type expression `{}`", init);

        lexer = l;
        compound.push(te);

        match lexer.next() {
            // End of compound type expession
            Some(Ok(Token::RParen)) => return ok!(lexer, compound), 
            // Next field in compound type expression
            Some(Ok(Token::Comma)) => continue,
            // Unexpected token
            Some(Ok(token)) => return err!(lexer, UnexpectedToken { 
                token, 
                slice: lexer.slice().into(), 
                span: lexer.span(), 
                expected: "`,` or `)`".to_string() 
            }).with_context(|lex| format!("parsing compound type expression `{}`", lexer_read_source(lex, init))),
            Some(Err(_)) => return err!(lexer, Unknown).with_context(|lex| format!("parsing compound type expression `{}`", lexer_read_source(lex, init))),
            None => return err!(lexer, UnexpectedEOF).with_context(|lex| format!("parsing compound type expression `{}`", lexer_read_source(lex, init))),
        }
    }
}