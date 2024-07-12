use crate::lex::{ast::{Expression, Identifier, Literal}, error::ParseError::*, lexer_read_source, token::Token::*, Lexer, PeekLexer};

use super::ParseResult;

pub fn parse_identifier(mut lexer: Lexer) -> ParseResult<Identifier> {
    match lexer.next() {
        Some(Ok(ValueIdentifier(s))) => ok!(lexer, Identifier::Value(s)),
        Some(Ok(TypeIdentifier(s))) => ok!(lexer, Identifier::Type(s)),
        Some(Ok(TagIdentifier(s))) => ok!(lexer, Identifier::Tag(s)),
        Some(Ok(token)) => err!(lexer, UnexpectedToken { 
            token, 
            slice: lexer.slice().into(), 
            span: lexer.span(), 
            expected: "identifier".to_string() 
        }),
        Some(Err(_)) => err!(lexer, Unknown),
        None => err!(lexer, UnexpectedEOF),
    }
}

pub fn parse_value_identifier(mut lexer: Lexer) -> ParseResult<Identifier> {
    match lexer.next() {
        Some(Ok(ValueIdentifier(s))) => ok!(lexer, Identifier::Value(s)),
        Some(Ok(token)) => err!(lexer, UnexpectedToken { 
            token, 
            slice: lexer.slice().into(), 
            span: lexer.span(), 
            expected: "value identifier".to_string() 
        }),
        Some(Err(_)) => err!(lexer, Unknown),
        None => err!(lexer, UnexpectedEOF),
    }
}

pub fn parse_type_identifier(mut lexer: Lexer) -> ParseResult<Identifier> {
    match lexer.next() {
        Some(Ok(TypeIdentifier(s))) => ok!(lexer, Identifier::Type(s)),
        Some(Ok(token)) => err!(lexer, UnexpectedToken { 
            token, 
            slice: lexer.slice().into(), 
            span: lexer.span(), 
            expected: "type identifier".to_string() 
        }),
        Some(Err(_)) => err!(lexer, Unknown),
        None => err!(lexer, UnexpectedEOF),
    }
}

pub fn parse_tag_identifier(mut lexer: Lexer) -> ParseResult<Identifier> {
    match lexer.next() {
        Some(Ok(TagIdentifier(s))) => ok!(lexer, Identifier::Tag(s)),
        Some(Ok(token)) => err!(lexer, UnexpectedToken { 
            token, 
            slice: lexer.slice().into(), 
            span: lexer.span(), 
            expected: "tag identifier".to_string() 
        }),
        Some(Err(_)) => err!(lexer, Unknown),
        None => err!(lexer, UnexpectedEOF),
    }
}

pub fn parse_literal(mut lexer: Lexer) -> ParseResult<Literal> {
    match lexer.next() {
        Some(Ok(IntegerLiteral(i))) => ok!(lexer, Literal::Integer(i)),
        Some(Ok(FloatLiteral(f))) => ok!(lexer, Literal::Float(f)),
        Some(Ok(StringLiteral(s))) => ok!(lexer, Literal::String(s)),
        Some(Ok(token)) => err!(lexer, UnexpectedToken { 
            token, 
            slice: lexer.slice().into(), 
            span: lexer.span(), 
            expected: "literal".to_string() 
        }),
        Some(Err(_)) => err!(lexer, Unknown),
        None => err!(lexer, UnexpectedEOF),
    }
}


/// Parse an expression
/// 
/// Expressions in Aura may be parenthized, literals, identifiers, calls, blocks or binary operations.
pub fn parse_expression(mut lexer: Lexer) -> ParseResult<Expression> {
    let init = lexer.span().end;
    match lexer.next() {
        Some(Ok(IntegerLiteral(i))) => ok!(lexer, Expression::Literal(Literal::Integer(i))),
        Some(Ok(FloatLiteral(f))) => ok!(lexer, Expression::Literal(Literal::Float(f))),
        Some(Ok(StringLiteral(s))) => ok!(lexer, Expression::Literal(Literal::String(s))),
        Some(Ok(ValueIdentifier(s))) => ok!(lexer, Expression::Identifier(Identifier::Value(s))),
        Some(Ok(LParen)) => parse_compound_expression(lexer)
            .map(|e| Expression::Compound(e))
            .with_context(|lex| format!("parsing compound expression at `{}`", lexer_read_source(lex, init))),
        Some(Ok(LBrace)) => block_expression(lexer)
            .map(|e| Expression::Block(e))
            .with_context(|lex| format!("parsing block expression at `{}`", lexer_read_source(lex, init))),
        Some(Ok(token)) => err!(lexer, UnexpectedToken { 
            token, 
            slice: lexer.slice().into(), 
            span: lexer.span(), 
            expected: "expression".to_string() 
        }),
        Some(Err(_)) => err!(lexer, Unknown),
        None => err!(lexer, UnexpectedEOF),
    }
}

pub fn parse_compound_expression(mut lexer: Lexer) -> ParseResult<Vec<Expression>> {
    let mut compound = vec![];
    let init = lexer.span().start;            
    
    loop {
        // Check if the next token is a right parenthesis
        // If so, return the compound expression
        if let (peeker, Some(Ok(RParen))) = lexer.peek() {
            return ok!(peeker, compound)
        }

        let (l, e) = withctx!(parse_expression(lexer), "parsing compound expression `{}`", init);

        lexer = l;
        compound.push(e);

        match lexer.next() {
            // End of compound expression
            Some(Ok(RParen)) => return ok!(lexer, compound), 
            // Next field in compound expression
            Some(Ok(Comma)) => continue,
            // Unexpected token
            Some(Ok(token)) => return err!(lexer, UnexpectedToken { 
                token, 
                slice: lexer.slice().into(), 
                span: lexer.span(), 
                expected: "`,` or `)`".to_string() 
            }).with_context(|lex| format!("parsing compound expression `{}`", lexer_read_source(lex, init))),
            Some(Err(_)) => return err!(lexer, Unknown),
            None => return err!(lexer, UnexpectedEOF),
        }
    }
}

pub fn block_expression(mut lexer: Lexer) -> ParseResult<Vec<Expression>> {
    let mut block = vec![];
    let init = lexer.span().start;            
    
    loop {
        // Check if the next token is a right parenthesis
        // If so, return the compound expression
        if let (peeker, Some(Ok(RBrace))) = lexer.peek() {
            return ok!(peeker, block)
        }

        let (l, e) = withctx!(parse_expression(lexer), "parsing block expression `{}`", init);

        lexer = l;
        block.push(e);

        match lexer.next() {
            // End of compound expression
            Some(Ok(RBrace)) => return ok!(lexer, block), 
            // Next field in compound expression
            Some(Ok(Semicolon)) => continue,
            // Unexpected token
            Some(Ok(token)) => return err!(lexer, UnexpectedToken { 
                token, 
                slice: lexer.slice().into(), 
                span: lexer.span(), 
                expected: "`,` or `}`".to_string() 
            }).with_context(|lex| format!("parsing block expression `{}`", lexer_read_source(lex, init))),
            Some(Err(_)) => return err!(lexer, Unknown),
            None => return err!(lexer, UnexpectedEOF),
        }
    }
}