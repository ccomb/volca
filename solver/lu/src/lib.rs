//! Solves `M x = b` for a sparse square matrix whose diagonal can serve as
//! pivots, which is what a matrix "identity minus exchanges" offers: every
//! column keeps its own product on the diagonal.
//!
//! The matrix is reordered once, symmetrically, so that the diagonal stays the
//! diagonal. For now the crate stops there: it says in which order the columns
//! would be eliminated.
#![forbid(unsafe_code)]

mod assemble;
mod order;

pub use order::Order;

/// A row or column as the caller numbers it, before any reordering.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Original(pub usize);

/// Why a computation did not happen: says nothing about the matrix.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Failure {
    OutOfMemory,
    /// A size does not fit the index types of the factors.
    TooLarge,
    /// The entry at this position names a row or column outside the matrix, or
    /// carries a value that is not a finite number.
    BadEntry(usize),
    /// Rows, columns and values are not three lists of one length.
    BadShape,
}

/// The order in which to eliminate the columns of the `n` by `n` matrix that
/// holds `vals[k]` at row `rows[k]` and column `cols[k]`.
pub fn elimination_order(
    n: usize,
    rows: &[u32],
    cols: &[u32],
    vals: &[f64],
) -> Result<Order, Failure> {
    order::order(&assemble::columns(n, rows, cols, vals)?)
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
