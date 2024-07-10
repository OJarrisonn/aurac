use logos::Span;
use thiserror::Error;

use super::token::Token;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("unexpected token `{token:?}` as `{slice}` at {span:?}, expected {expected}")]
    UnexpectedToken {
        token: Token,
        slice: String,
        span: Span,
        expected: String,
    },
    #[error("unexpected identifier `{identifier}` at {span:?}, expected {expected}")]
    UnexpectedIdentifier {
        identifier: String,
        span: Span,
        expected: String,
    },
    #[error("unexpected EOF")]
    UnexpectedEOF,
    #[error("unknown error")]
    Unknown,
}