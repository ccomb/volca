//! Solves `M x = b` for a sparse square matrix whose diagonal can serve as
//! pivots, which is what a matrix "identity minus exchanges" offers: every
//! column keeps its own product on the diagonal.
//!
//! The matrix is reordered once, symmetrically, so that the diagonal stays the
//! diagonal; rows are never exchanged afterwards. A matrix that would need an
//! exchange is refused rather than factorized badly.
#![forbid(unsafe_code)]

mod assemble;
mod block;
mod dense;
mod factor;
mod order;
mod solve;

pub use block::{Factors, View};
pub use factor::{Report, Verdict, Weakest, factorize};
pub use solve::scratch_len;

/// A row or column as the caller numbers it, before any reordering.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Original(pub usize);

/// Why a matrix is not factorized: a statement about the matrix, which stays
/// true however often the caller asks.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Refusal {
    /// The eliminations before this column cancelled out its diagonal entry:
    /// what is left of it cannot serve as a pivot.
    TinyPivot { column: Original, pivot: f64 },
    /// A trial solve with the factors leaves this share of a row unexplained.
    Unreliable { error: f64 },
}

/// Why a computation did not happen: says nothing about the matrix.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Failure {
    OutOfMemory,
    /// A size does not fit the index types of the factors.
    TooLarge,
    /// The entry at this position names a row or column outside the matrix, or
    /// carries a value that is not a finite number.
    BadEntry(usize),
    /// The bytes handed back are not factors this crate wrote.
    BadBlock,
    /// Lists that go together differ in length, or one is too short.
    BadShape,
}

/// A vector of `len` copies of `value`, or an error where the allocator would
/// otherwise abort the process.
pub(crate) fn filled<T: Clone>(len: usize, value: T) -> Result<Vec<T>, Failure> {
    let mut out = reserved(len)?;
    out.resize(len, value);
    Ok(out)
}

/// An empty vector with room for `len` values, under the same condition.
pub(crate) fn reserved<T>(len: usize) -> Result<Vec<T>, Failure> {
    let mut out = Vec::new();
    out.try_reserve_exact(len)
        .map_err(|_| Failure::OutOfMemory)?;
    Ok(out)
}

/// Room for `more` values at the end of `v`, under the same condition.
pub(crate) fn room<T>(v: &mut Vec<T>, more: usize) -> Result<(), Failure> {
    v.try_reserve(more).map_err(|_| Failure::OutOfMemory)
}
