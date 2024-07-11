use fn_::parse_function_signature;

use crate::lex::{ast::{Expression, Fn, Identifier, Main, Mod, Type, TypeExpression, Val}, error::ParseError::*, lexer_read_source, take_exact, token::Token, Lexer, PeekLexer};

use super::{expr::{parse_expression, parse_literal, parse_type_identifier, parse_value_identifier}, types::parse_type, ParseResult};

pub mod fn_;

/// Parse a module
/// A module is an Aura source code file with global definitions for the module
pub fn parse_mod(mut lexer: Lexer) -> ParseResult<Mod> {
    let mut module = Mod::default();
    
    loop {
        let (_, future_token) = lexer.peek();

        match future_token {
            Some(Ok(Token::Val)) => {
                let (l, v) = withctx!(parse_val_defn(lexer), "parsing `val` definition in module");
                lexer = l;
                module.vals.push(v);
            },
            Some(Ok(Token::Type)) => {
                let (l, t) = withctx!(parse_type_defn(lexer), "parsing `type` definition in module");
                lexer = l;
                module.types.push(t);
            },
            Some(Ok(Token::Fn)) => {
                let (l, f) = withctx!(parse_fn_defn(lexer), "parsing `fn` definition in module");
                lexer = l;
                module.fns.push(f);
            },
            Some(Ok(Token::Main)) => {
                let (l, m) = withctx!(parse_main_defn(lexer), "parsing `main` definition in module");
                lexer = l;
                module.main = Some(m);
            },
            Some(Ok(token)) => return err!(lexer, UnexpectedToken { 
                token, 
                slice: lexer.slice().into(), 
                span: lexer.span(), 
                expected: "`val` definition".to_string() 
            }).context("parsing module"),
            Some(Err(_)) => return err!(lexer, Unknown).context("parsing module"),
            None => break,
        };
    }

    ok!(lexer, module)
}

/// Parse a `val` declaration
/// 
/// Syntax: 
/// ```ignore
/// val symbol Type = expression;
/// ```
pub fn parse_val_defn(lexer: Lexer) -> ParseResult<Val> {
    // `val`
    let (lexer, _) = withctx!(take_exact(lexer, Token::Val), "expected `val` definition"); 

    // `symbol`
    let (lexer, symbol) = withctx!(parse_value_identifier(lexer), "parsing the symbol of a `val` definition");

    // `Type`
    let (lexer, type_) = withctx!(parse_type(lexer), "parsing type in `val` definition");

    // `=`
    let (lexer, _) = withctx!(take_exact(lexer, Token::Assign), "parsing `=` in `val` definition");

    // `expression`
    let (lexer, value) = withctx!(parse_literal(lexer), "parsing literal expression in `val` definition");

    ok!(lexer, Val { symbol, type_, value })
}

pub fn parse_type_defn(lexer: Lexer) -> ParseResult<Type> {
    let (lexer, _) = withctx!(take_exact(lexer, Token::Type), "expected `type` definition");
    let (lexer, symbol) = withctx!(parse_type_identifier(lexer), "parsing the symbol of a `type` definition");
    let (lexer, _) = withctx!(take_exact(lexer, Token::Assign), "parsing `=` in `type` definition");
    let (lexer, type_) = withctx!(parse_type(lexer), "parsing type in `type` definition");

    ok!(lexer, Type { symbol, type_ })
}

pub fn parse_fn_defn(lexer: Lexer) -> ParseResult<Fn> {
    let (lexer, _) = withctx!(take_exact(lexer, Token::Fn), "expected `fn` definition");

    let (lexer, symbol) = withctx!(parse_value_identifier(lexer), "parsing the symbol of a `fn` definition");

    let (mut lexer, (input, output)) = withctx!(parse_function_signature(lexer), "parsing function signature in `fn` definition");

    // Foresee the next token if it's a `=` or `{`
    let allow_non_block = match lexer.peek() {
        (peeker, Some(Ok(Token::Assign))) => {
            lexer = peeker;
            true
        },
        (_, Some(Ok(Token::LBrace))) => false,
        (_, Some(Ok(token))) => return err!(lexer, UnexpectedToken { 
            token, 
            slice: lexer.slice().into(), 
            span: lexer.span(), 
            expected: "`{` or `=`".to_string() 
        }).context("parsing `fn` definition"),
        (_, Some(Err(_))) => return err!(lexer, Unknown).context("parsing `fn` definition"),
        (_, None) => return err!(lexer, UnexpectedEOF).context("parsing `fn` definition"),
    };

    let (lexer, body) = withctx!(parse_expression(lexer), "parsing expression in `fn` definition");

    // If allow_non_block is true then the body is any expression
    // If allow_non_block is false then the body is a block expression
    if !allow_non_block {
        match body {
            Expression::Block(_) => (),
            _ => return err!(lexer, UnexpectedToken { 
                token: Token::LBrace, 
                slice: lexer.slice().into(), 
                span: lexer.span(), 
                expected: "block expression or `=`".to_string() 
            }).context("parsing `fn` definition"),
        }
    }

    ok!(lexer, Fn { symbol, input, output, body })
}

pub fn parse_main_defn(lexer: Lexer) -> ParseResult<Main> {
    let init = lexer.span().end;
    
    let (lexer, _) = withctx!(take_exact(lexer, Token::Main), "expected `main` definition in {}", init);

    // `(`, `->`, `=` or `{`
    let (lexer, (input, output)) = match lexer.peek().1 {
        Some(Ok(Token::LParen)) | Some(Ok(Token::Arrow)) => withctx!(parse_function_signature(lexer), "parsing function signature in `main` definition in {}", init),
        Some(Ok(Token::Assign)) | Some(Ok(Token::LBrace)) => (lexer, (vec![], TypeExpression::Identifier(Identifier::Type("Void".to_string())))),
        Some(Ok(token)) => return err!(lexer, UnexpectedToken { 
            token, 
            slice: lexer.slice().into(), 
            span: lexer.span(), 
            expected: "`(` or `=`".to_string() 
        }).with_context(|lex| format!("parsing `main` definition in {}", lexer_read_source(lex, init))),
        Some(Err(_)) => return err!(lexer, Unknown).with_context(|lex| format!("parsing `main` definition in {}", lexer_read_source(lex, init))),
        None => return err!(lexer, UnexpectedEOF).with_context(|lex| format!("parsing `main` definition in {}", lexer_read_source(lex, init))),
    };

    // Check for a `=` to allow non `{ }` expressions as the body
    let (lexer, allow_non_block) = if let (lexer, Some(Ok(Token::Assign))) = lexer.peek() { 
        (lexer, true) 
    } else { 
        (lexer, false) 
    };

    let (lexer, body) = withctx!(parse_expression(lexer), "parsing expression in `main` definition");

    if !allow_non_block {
        match body {
            Expression::Block(_) => (),
            _ => return err!(lexer, UnexpectedToken { 
                token: Token::LBrace, 
                slice: lexer.slice().into(), 
                span: lexer.span(), 
                expected: "block expression or `=`".to_string() 
            }).context("parsing `main` definition"),
        }
    }

    ok!(lexer, Main { input, output, body })
}