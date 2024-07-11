use std::fmt::Debug;

use anyhow::{Context, Result};

use crate::utils::{FromFailure, Propagate};

use super::Lexer;

pub mod expr;
pub mod defn;
pub mod types;

#[allow(dead_code)]
type ParserFn<'source, T> = fn(Lexer<'source>) -> ParseResult<'source, T>;

/// A parse result yield from every parse function
/// 
/// This struct contains the lexer after parsing and the result of the parse function
#[derive(Debug)]
pub struct ParseResult<'source, T> {
    pub lexer: Lexer<'source>,
    pub result: Result<T>
}

impl <'source, T: Debug> ParseResult<'source, T> {
    /// Create a new parse result
    pub fn new(result: Result<T>, lexer: Lexer<'source>) -> Self {
        Self {
            lexer,
            result
        }
    }

    pub fn ok(result: T, lexer: Lexer<'source>) -> Self {
        Self {
            lexer,
            result: Ok(result)
        }
    }

    pub fn err(err: anyhow::Error, lexer: Lexer<'source>) -> Self {
        Self {
            lexer,
            result: Err(err)
        }
    }

    /// Takes the result from the parse result
    pub fn result(self) -> Result<T> {
        self.result
    }

    /// Takes the lexer from the parse result
    pub fn lexer(self) -> Lexer<'source> {
        self.lexer
    }

    /// Check if the parse result is ok
    pub fn is_ok(&self) -> bool {
        self.result.is_ok()
    }

    /// Check if the parse result is an error
    pub fn is_err(&self) -> bool {
        self.result.is_err()
    }

    /// Unwrap the parse result into a tuple of the lexer and the ok result
    pub fn unwrap(self) -> (Lexer<'source>, T) {
        (self.lexer, self.result.unwrap())
    }

    /// Unwrap the error result into a tuple of the lexer and the err result
    pub fn unwrap_err(self) -> (Lexer<'source>, anyhow::Error) {
        (self.lexer, self.result.unwrap_err())
    }

    /// Map the result of the parse result
    pub fn map<U, F>(self, f: F) -> ParseResult<'source, U> 
    where 
        F: FnOnce(T) -> U
    {
        ParseResult {
            lexer: self.lexer,
            result: self.result.map(f)
        }
    }


    /// Pack the parse result into a tuple of the lexer and the result
    pub fn pack(self) -> Result<(Lexer<'source>, T)> {
        self.result.map(|r| (self.lexer, r))
    }

    /// Add a context to the error message handled by anyhow
    /// 
    /// The context callback has access to the lexer wrapped in the result
    pub fn with_context<F, C>(self, context: F) -> Self 
    where 
        F: FnOnce(&Lexer) -> C,
        C: std::fmt::Display + Send + Sync + 'static
    {
        Self {
            result: self.result.with_context(|| context(&self.lexer)),
            lexer: self.lexer,
        }
    }

    /// Add a context to the error message handled by anyhow
    pub fn context<C>(self, context: C) -> Self
    where 
        C: std::fmt::Display + Send + Sync + 'static
    {
        Self {
            result: self.result.context(context),
            lexer: self.lexer,
        }
    }
}

impl<'source, T: Debug> FromFailure<(Lexer<'source>, anyhow::Error)> for ParseResult<'source, T> {
    fn from(f: (Lexer<'source>, anyhow::Error)) -> Self {
        Self::err(f.1, f.0)
    }
}

impl<'source, T: Debug> Propagate<(Lexer<'source>, anyhow::Error)> for ParseResult<'source, T> {
    type Success = (Lexer<'source>, T);

    type Failure = (Lexer<'source>, anyhow::Error);

    fn is_success(&self) -> bool {
        self.is_ok()
    }

    fn unwrap(self) -> Self::Success {
        self.unwrap()
    }

    fn unwrap_fail(self) -> Self::Failure {
        self.unwrap_err()
    }
}