use anyhow::{Result, bail, Context};

use crate::lex::{ast::{Expression, Identifier, Literal}, error::ParseError::*, token::Token::*, Lexer, PeekLexer};

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

pub fn parse_expression(mut lexer: Lexer) -> Result<(Lexer, Expression)> {
    match lexer.next() {
        Some(Ok(IntegerLiteral(i))) => Ok((lexer, Expression::Literal(Literal::Integer(i)))),
        Some(Ok(FloatLiteral(f))) => Ok((lexer, Expression::Literal(Literal::Float(f)))),
        Some(Ok(StringLiteral(s))) => Ok((lexer, Expression::Literal(Literal::String(s)))),
        Some(Ok(ValueIdentifier(s))) => Ok((lexer, Expression::Identifier(Identifier::Value(s)))),
        Some(Ok(LParen)) => {
            let src = lexer.source()[lexer.span().start..lexer.span().end].trim();
            parse_compound_expression(lexer)
            .map(|(l, e)| (l, Expression::Compound(e)))
            .with_context(|| format!("parsing compound expression at `{}`", src))
        },
        Some(Ok(LBrace)) => {
            let src = lexer.source()[lexer.span().start..lexer.span().end].trim();
            block_expression(lexer)
            .map(|(l, e)| (l, Expression::Block(e)))
            .with_context(|| format!("parsing block expression at `{}`", src))
        },
        Some(Ok(token)) => bail!(UnexpectedToken { 
            token, 
            slice: lexer.slice().into(), 
            span: lexer.span(), 
            expected: "expression".to_string() 
        }),
        Some(Err(_)) => bail!(Unknown),
        None => bail!(UnexpectedEOF),
    }
}

pub fn parse_compound_expression(mut lexer: Lexer) -> Result<(Lexer, Vec<Expression>)> {
    let mut compound = vec![];
    let init = lexer.span().start;            
    
    loop {
        // Check if the next token is a right parenthesis
        // If so, return the compound expression
        if let (peeker, Some(Ok(RParen))) = lexer.peek() {
            return Ok((peeker, compound))
        }

        let (l, e) = {
            let src = lexer.source()[init..lexer.span().end].trim();
            parse_expression(lexer).with_context(|| format!("parsing compound expression `{}`", src))?
        };

        lexer = l;
        compound.push(e);

        match lexer.next() {
            // End of compound expression
            Some(Ok(RParen)) => return Ok((lexer, compound)), 
            // Next field in compound expression
            Some(Ok(Comma)) => continue,
            // Unexpected token
            Some(Ok(token)) => return Err(UnexpectedToken { 
                token, 
                slice: lexer.slice().into(), 
                span: lexer.span(), 
                expected: "`,` or `)`".to_string() 
            }).with_context(|| format!("parsing compound expression `{}`", lexer.source()[init..lexer.span().end].trim())),
            Some(Err(_)) => bail!(Unknown),
            None => bail!(UnexpectedEOF),
        }
    }
}

pub fn block_expression(mut lexer: Lexer) -> Result<(Lexer, Vec<Expression>)> {
    let mut block = vec![];
    let init = lexer.span().start;            
    
    loop {
        // Check if the next token is a right parenthesis
        // If so, return the compound expression
        if let (peeker, Some(Ok(RBrace))) = lexer.peek() {
            return Ok((peeker, block))
        }

        let (l, e) = {
            let src = lexer.source()[init..lexer.span().end].trim();
            parse_expression(lexer).with_context(|| format!("parsing block expression `{}`", src))?
        };

        lexer = l;
        block.push(e);

        match lexer.next() {
            // End of compound expression
            Some(Ok(RBrace)) => return Ok((lexer, block)), 
            // Next field in compound expression
            Some(Ok(Semicolon)) => continue,
            // Unexpected token
            Some(Ok(token)) => return Err(UnexpectedToken { 
                token, 
                slice: lexer.slice().into(), 
                span: lexer.span(), 
                expected: "`,` or `}`".to_string() 
            }).with_context(|| format!("parsing block expression `{}`", lexer.source()[init..lexer.span().end].trim())),
            Some(Err(_)) => bail!(Unknown),
            None => bail!(UnexpectedEOF),
        }
    }
}