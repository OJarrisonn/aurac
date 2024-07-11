use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<L, R> Either<L, R> {
    pub fn swap(self) -> Either<R, L> {
        match self {
            Either::Left(l) => Either::Right(l),
            Either::Right(r) => Either::Left(r),
        }
    }

    pub fn left(&self) -> Option<&L> {
        match self {
            Either::Left(l) => Some(l),
            Either::Right(_) => None,
        }
    }

    pub fn left_mut(&mut self) -> Option<&mut L> {
        match self {
            Either::Left(l) => Some(l),
            Either::Right(_) => None,
        }
    }

    pub fn right(&self) -> Option<&R> {
        match self {
            Either::Left(_) => None,
            Either::Right(r) => Some(r),
        }
    }

    pub fn right_mut(&mut self) -> Option<&mut R> {
        match self {
            Either::Left(_) => None,
            Either::Right(r) => Some(r),
        }
    }

    pub fn is_left(&self) -> bool {
        match self {
            Either::Left(_) => true,
            Either::Right(_) => false,
        }
    }

    pub fn is_right(&self) -> bool {
        match self {
            Either::Left(_) => false,
            Either::Right(_) => true,
        }
    }

    pub fn map_left<F, T>(self, f: F) -> Either<T, R> 
    where
        F: FnOnce(L) -> T,
    {
        match self {
            Either::Left(l) => Either::Left(f(l)),
            Either::Right(r) => Either::Right(r),
        }
    }

    pub fn map_right<F, T>(self, f: F) -> Either<L, T> 
    where
        F: FnOnce(R) -> T,
    {
        match self {
            Either::Left(l) => Either::Left(l),
            Either::Right(r) => Either::Right(f(r)),
        }
    }
}

impl<L, R> Display for Either<L, R> 
where
    L: Display,
    R: Display,
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Either::Left(l) => write!(f, "{}", l),
            Either::Right(r) => write!(f, "{}", r),
        }
    }
}

impl<L, R> From<Result<L, R>> for Either<L, R> {
    fn from(result: Result<L, R>) -> Self {
        match result {
            Ok(l) => Either::Left(l),
            Err(r) => Either::Right(r),
        }
    }
}

impl<L, R> From<Either<L, R>> for Result<L, R> {
    fn from(either: Either<L, R>) -> Self {
        match either {
            Either::Left(l) => Ok(l),
            Either::Right(r) => Err(r),
        }
    }
}

/// alias for `propagate!`
/// 
/// This trait is used to unwrap successful values or short circuit the function returning the error
pub trait Propagate<F>: FromFailure<F> {
    /// The type that is unwraped if the value is a success
    type Success;
    /// The type that is unwraped if the value is a failure
    /// The type implementing Propagate must implement FromFailure<Self::Failure>
    type Failure;

    /// Checks if the value is a success
    fn is_success(&self) -> bool;

    /// Checks if the value is a failure
    fn is_failure(&self) -> bool {
        !self.is_success()
    }

    /// Unwraps the value if it is a success
    /// 
    /// # Panics
    /// If the value isn't a success
    fn unwrap(self) -> Self::Success;

    /// Unwraps the value if it is a failure
    /// 
    /// # Panics
    /// If the value isn't a failure
    fn unwrap_fail(self) -> Self::Failure;
}

/// This trait is used to convert a failure into a value
/// 
/// Is expected that if T implements FromFailure<F> then T implements Propagate<F> and T::Failure == F
/// 
/// This is used by the `propagate!` macro to convert the failure into a value and let the type inferece change the Success value
pub trait FromFailure<F> {
    /// Convert the failure into a value
    fn from(f: F) -> Self;
}