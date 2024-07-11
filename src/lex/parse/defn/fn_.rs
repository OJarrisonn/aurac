use crate::lex::{ast::{Identifier, TypeExpression}, error::ParseError::*, parse::{expr::parse_value_identifier, types::parse_type, ParseResult}, take_exact, token::Token, Lexer, PeekLexer};


/// Parse a function signature in a function definition
/// 
/// Syntax:
///     ( ValueIdentifier TypeExpression [, ValueIdentifier TypeExpression]* ) -> TypeExpression
/// TODO: Show where the error happened in the source code
pub fn parse_function_signature(lexer: Lexer) -> ParseResult<(Vec<(Identifier, TypeExpression)>, TypeExpression)> {
    if let (peeker, Some(Ok(Token::Arrow))) = lexer.peek() {
        if let (_, Some(Ok(Token::Assign))) = peeker.peek() {
            return ok!(peeker, (vec![], TypeExpression::Identifier(Identifier::Type("Void".to_string()))))
        }

        if let (_, Some(Ok(Token::LBrace))) = peeker.peek() {
            return ok!(peeker, (vec![], TypeExpression::Identifier(Identifier::Type("Void".to_string()))))
        }

        let (lexer, output) = withctx!(parse_type(lexer), "parsing function return type");

        return ok!(lexer, (vec![], output))
    }
    let (mut lexer, _) = withctx!(take_exact(lexer, Token::LParen), "expected `(` at the start of a function signature");

    let mut params = vec![];

    loop {
        if let (peeker, Some(Ok(Token::RParen))) = lexer.peek() {
            lexer = peeker;
            break
        }

        let (l, id) = withctx!(parse_value_identifier(lexer), "parsing function parameter identifier");
        let (mut l, ty) = withctx!(parse_type(l), "parsing function parameter type");

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
            Some(Ok(token)) => return err!(l, UnexpectedToken { 
                token, 
                slice: l.slice().into(), 
                span: l.span(), 
                expected: "`,` or `)`".to_string() 
            }).context("parsing function signature"),
            Some(Err(_)) => return err!(l, Unknown).context("parsing function signature"),
            None => return err!(l, UnexpectedEOF).context("parsing function signature"),
        }
    }

    // Implicit void return type
    if let (_, Some(Ok(Token::Assign))) = lexer.peek() {
        return ok!(lexer, (params, TypeExpression::Identifier(Identifier::Type("Void".to_string()))))
    }

    if let (_, Some(Ok(Token::LBrace))) = lexer.peek() {
        return ok!(lexer, (params, TypeExpression::Identifier(Identifier::Type("Void".to_string()))))
    }

    let (lexer, _) = withctx!(take_exact(lexer, Token::Arrow), "expected `->` in function signature");

    let (lexer, ret) = withctx!(parse_type(lexer), "parsing function return type");

    ok!(lexer, (params, ret))
}