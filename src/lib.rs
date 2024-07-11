/// Creates an Ok ParseResult
#[macro_export]
macro_rules! ok {
    ($lexer:expr, $ok:expr) => {
        crate::lex::parse::ParseResult::ok($ok, $lexer);
    };
    ($lexer:expr) => {
        crate::lex::parse::ParseResult::ok((), $lexer);
    };
}

/// Creates an Err ParseResult
#[macro_export]
macro_rules! err {
    ($lexer:expr, $err:expr) => {
        crate::lex::parse::ParseResult::err($err.into(), $lexer);
    };
}

#[macro_export]
macro_rules! withctx {
    ($result:expr, $ctx:literal, $init:expr) => {{
        let ParseResult { result, lexer } = $result.with_context(|lex| format!($ctx, lex.source()[$init..lex.span().end].trim()));

        match result {
            Ok(output) => (lexer, output),
            Err(err) => return err!(lexer, err)
        }
    }};
    ($result:expr, $ctx:expr) => {{
        let ParseResult { result, lexer } = $result.context($ctx);

        match result {
            Ok(output) => (lexer, output),
            Err(err) => return err!(lexer, err)
        }
    }};
}

#[macro_export]
macro_rules! propagate {
    ($value:expr) => {
        if (crate::utils::Propagate::is_success(&$value)) {
            crate::utils::Propagate::unwrap($value)
        } else {
            return crate::utils::FromFailure::from(crate::utils::Propagate::unwrap_fail($value))
        }
    };
}