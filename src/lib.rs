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

/// The propagate macro is used to unwrap a type that implements Propagate<F> if it's success or short circuit the function if it's a failure returning the error
/// 
/// Implement the trait `aurac::utils::Propagate` for your type to use this macro
#[macro_export]
macro_rules! propagate {
    ($value:expr) => {{
        let res = $value;

        if (crate::utils::Propagate::is_success(&res)) {
            crate::utils::Propagate::unwrap(res)
        } else {
            return crate::utils::FromFailure::from(crate::utils::Propagate::unwrap_fail(res))
        }
    }};
}