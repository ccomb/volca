//! The solver as the engine links it: a static archive with a C face.
//!
//! This is the only crate of the workspace where `unsafe` may appear, because
//! a symbol the linker sees and a pointer received from another language cannot
//! be written without it.
use lu::{Factors, Failure, Original, Refusal, Verdict, View};
use std::panic::{AssertUnwindSafe, catch_unwind, resume_unwind};

// What the functions below answer. The engine reads these numbers in
// `src/Matrix/Sparse.hs`: the two lists change together.
const DONE: u32 = 0;
const TINY_PIVOT: u32 = 1;
const UNRELIABLE: u32 = 2;
const OUT_OF_MEMORY: u32 = 3;
const TOO_LARGE: u32 = 4;
const BAD_ENTRY: u32 = 5;
const BAD_BLOCK: u32 = 6;
const BAD_SHAPE: u32 = 7;
const PANICKED: u32 = 8;
const WRONG_ANSWER: u32 = 9;
const UNWINDING_WENT_THROUGH: u32 = 10;

fn code(failure: Failure) -> u32 {
    match failure {
        Failure::OutOfMemory => OUT_OF_MEMORY,
        Failure::TooLarge => TOO_LARGE,
        Failure::BadEntry(_) => BAD_ENTRY,
        Failure::BadBlock => BAD_BLOCK,
        Failure::BadShape => BAD_SHAPE,
    }
}

/// Runs `work`, and answers for it if it panics: unwinding into the caller's
/// language would kill the process.
fn guarded(work: impl FnOnce() -> u32) -> u32 {
    catch_unwind(AssertUnwindSafe(work)).unwrap_or(PANICKED)
}

/// What a factorization found beyond its answer. Every field is eight bytes
/// wide, so the caller reads the one of rank `k` eight times `k` bytes in.
#[repr(C)]
pub struct Found {
    /// Done: the factors, to give once to `volca_lu_export` or `volca_lu_discard`.
    factors: *mut Factors,
    /// Done: how many bytes `volca_lu_export` writes.
    bytes: u64,
    /// Done: the column whose pivot kept the smallest share of the diagonal
    /// entry it started from, when the matrix has a column. Tiny pivot: its
    /// column. Bad entry: its position among the entries.
    at: u64,
    /// Done: that share. Tiny pivot: the pivot. Unreliable: the share of a
    /// row left unexplained.
    value: f64,
    /// Done: how many columns were eliminated as one full block.
    dense: u64,
    /// Done: the share of a row that the trial solve left unexplained.
    error: f64,
}

const _: () = assert!(size_of::<Found>() == 48);

/// The `len` items at `ptr`. An empty list may come with any pointer. One that
/// has items and no address to read them at is the caller's mistake: taken for
/// an empty list, it would be answered as if the work had been done.
///
/// # Safety
///
/// With `len` above zero, `ptr` is null or points to `len` initialized items
/// that nothing writes for as long as the result is used.
unsafe fn list<'a, T>(ptr: *const T, len: usize) -> Option<&'a [T]> {
    if len == 0 {
        Some(&[])
    } else if ptr.is_null() || !ptr.is_aligned() {
        None
    } else {
        // SAFETY: not null, aligned, and the rest is what the caller promises.
        Some(unsafe { std::slice::from_raw_parts(ptr, len) })
    }
}

/// The same, to write to.
///
/// # Safety
///
/// With `len` above zero, `ptr` is null or points to `len` initialized items
/// that nothing else reads or writes for as long as the result is used.
unsafe fn list_mut<'a, T>(ptr: *mut T, len: usize) -> Option<&'a mut [T]> {
    if len == 0 {
        Some(&mut [])
    } else if ptr.is_null() || !ptr.is_aligned() {
        None
    } else {
        // SAFETY: not null, aligned, and the rest is what the caller promises.
        Some(unsafe { std::slice::from_raw_parts_mut(ptr, len) })
    }
}

/// Factorizes the `n` by `n` matrix given by `entries` entries, the one of
/// rank `k` holding `vals[k]` at row `rows[k]` and column `cols[k]`, and says
/// in `found` what goes with the answer.
///
/// # Safety
///
/// `rows`, `cols` and `vals` each point to `entries` items that nothing writes
/// during the call, and `found` to a `Found` nothing else reads or writes.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn volca_lu_factorize(
    n: usize,
    entries: usize,
    rows: *const u32,
    cols: *const u32,
    vals: *const f64,
    found: *mut Found,
) -> u32 {
    // SAFETY: the caller's second promise.
    let Some(found) = (unsafe { found.as_mut() }) else {
        return BAD_SHAPE;
    };
    *found = Found {
        factors: std::ptr::null_mut(),
        bytes: 0,
        at: 0,
        value: 0.0,
        dense: 0,
        error: 0.0,
    };
    // SAFETY: the caller's first promise.
    let lists = unsafe {
        (
            list(rows, entries),
            list(cols, entries),
            list(vals, entries),
        )
    };
    let (Some(rows), Some(cols), Some(vals)) = lists else {
        return BAD_SHAPE;
    };
    guarded(|| match lu::factorize(n, rows, cols, vals) {
        Ok(Verdict::Accepted(factors, report)) => {
            if let Some((Original(column), share)) = report.weakest {
                found.at = column as u64;
                found.value = share;
            }
            found.dense = report.dense_columns as u64;
            found.error = report.probe_error;
            found.bytes = factors.byte_len() as u64;
            found.factors = Box::into_raw(factors);
            DONE
        }
        Ok(Verdict::Refused(Refusal::TinyPivot { column, pivot })) => {
            let Original(column) = column;
            found.at = column as u64;
            found.value = pivot;
            TINY_PIVOT
        }
        Ok(Verdict::Refused(Refusal::Unreliable { error })) => {
            found.value = error;
            UNRELIABLE
        }
        Err(failure) => {
            if let Failure::BadEntry(at) = failure {
                found.at = at as u64;
            }
            code(failure)
        }
    })
}

/// Copies the factors into the `len` bytes at `block`, where
/// `volca_lu_solve` reads them, and frees them whatever the answer.
///
/// # Safety
///
/// `factors` is null or comes from `volca_lu_factorize` and has been given
/// neither to this function nor to `volca_lu_discard` before. `block` points
/// to `len` bytes nothing else reads or writes during the call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn volca_lu_export(factors: *mut Factors, block: *mut u8, len: usize) -> u32 {
    if factors.is_null() {
        return BAD_BLOCK;
    }
    // SAFETY: the caller's first promise: this is the one owner of the factors.
    let factors = unsafe { Box::from_raw(factors) };
    // SAFETY: the caller's second promise.
    let Some(block) = (unsafe { list_mut(block, len) }) else {
        return BAD_SHAPE;
    };
    guarded(|| match factors.write(block) {
        Ok(()) => DONE,
        Err(failure) => code(failure),
    })
}

/// Frees factors that will not be exported.
///
/// # Safety
///
/// `factors` is null or comes from `volca_lu_factorize` and has been given
/// neither to this function nor to `volca_lu_export` before.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn volca_lu_discard(factors: *mut Factors) {
    if !factors.is_null() {
        // SAFETY: the caller's promise: this is the one owner of the factors.
        drop(unsafe { Box::from_raw(factors) });
    }
}

/// How many numbers of working space `volca_lu_solve` takes to solve
/// `columns` columns with the factors of a matrix `n` wide.
#[unsafe(no_mangle)]
pub extern "C" fn volca_lu_scratch_len(n: usize, columns: usize) -> usize {
    lu::scratch_len(n, columns)
}

/// Replaces each column of `rhs` (one after the other, each as long as the
/// matrix is wide) by the solution for it, with the factors
/// `volca_lu_export` wrote in the `len` bytes at `block`. Writes nothing to
/// the block: any number of threads may solve with the same one at once.
///
/// # Safety
///
/// `block` points to `len` bytes nothing writes during the call. `rhs` points
/// to `rhs_len` numbers and `scratch` to `scratch_len` numbers that nothing
/// else reads or writes during the call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn volca_lu_solve(
    block: *const u8,
    len: usize,
    rhs: *mut f64,
    rhs_len: usize,
    scratch: *mut f64,
    scratch_len: usize,
) -> u32 {
    // SAFETY: the caller's promises, one for each list.
    let lists = unsafe {
        (
            list(block, len),
            list_mut(rhs, rhs_len),
            list_mut(scratch, scratch_len),
        )
    };
    let (Some(block), Some(rhs), Some(scratch)) = lists else {
        return BAD_SHAPE;
    };
    guarded(
        || match View::read(block).and_then(|factors| factors.solve(rhs, scratch)) {
            Ok(()) => DONE,
            Err(failure) => code(failure),
        },
    )
}

/// Says whether this archive works once linked into a foreign program, which a
/// build that merely succeeds does not show: its code runs, the allocator
/// answers, and a panic stops here instead of killing the caller.
// SAFETY: the name is prefixed, and nothing else the engine links defines it.
#[unsafe(no_mangle)]
pub extern "C" fn volca_solver_probe() -> u32 {
    guarded(probe)
}

fn probe() -> u32 {
    // x + y/2 = 2 and x/2 + y = 2.5 have x = 1 and y = 2 for solution.
    let (rows, cols, vals) = ([0, 1, 0, 1], [0, 1, 1, 0], [1.0, 1.0, 0.5, 0.5]);
    let mut x = [2.0, 2.5];
    let solved = match lu::factorize(2, &rows, &cols, &vals) {
        Ok(Verdict::Accepted(factors, _)) => factors.view().solve(&mut x, &mut [0.0; 2]),
        _ => return WRONG_ANSWER,
    };
    if solved.is_err() || (x[0] - 1.0).abs() > 1e-15 || (x[1] - 2.0).abs() > 1e-15 {
        return WRONG_ANSWER;
    }
    // Unwinds like a panic, without the message a panic prints.
    match catch_unwind(|| resume_unwind(Box::new(()))) {
        Err(_) => DONE,
        Ok(()) => UNWINDING_WENT_THROUGH,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_probe_is_sound_where_it_was_built() {
        assert_eq!(volca_solver_probe(), DONE);
    }

    const EMPTY: Found = Found {
        factors: std::ptr::null_mut(),
        bytes: 0,
        at: 0,
        value: 0.0,
        dense: 0,
        error: 0.0,
    };

    /// A full matrix of this many columns, one on the diagonal and little off
    /// it: large enough to be eliminated as a full block.
    fn full(n: u32) -> (Vec<u32>, Vec<u32>, Vec<f64>) {
        let pairs = || (0..n).flat_map(|j| (0..n).map(move |i| (i, j)));
        let value = |(i, j): (u32, u32)| {
            if i == j {
                1.0
            } else {
                ((i * 7 + j * 13) % 11) as f64 / 4000.0 - 0.001
            }
        };
        (
            pairs().map(|(i, _)| i).collect(),
            pairs().map(|(_, j)| j).collect(),
            pairs().map(value).collect(),
        )
    }

    #[test]
    fn a_matrix_goes_through_the_four_calls_and_comes_back_solved() {
        let n = 300;
        let (rows, cols, vals) = full(n as u32);
        let mut found = EMPTY;
        // SAFETY: three lists of one length, and a `Found` of this test's own.
        let answer = unsafe {
            volca_lu_factorize(
                n,
                vals.len(),
                rows.as_ptr(),
                cols.as_ptr(),
                vals.as_ptr(),
                &mut found,
            )
        };
        assert_eq!(answer, DONE);
        assert!(found.dense > 0 && (found.at as usize) < n);
        assert!((found.value - 1.0).abs() < 0.01, "{}", found.value);
        assert!(found.error < 1e-13, "{}", found.error);
        let mut block = vec![0u64; found.bytes as usize / 8];
        // SAFETY: the factors just received, given once, and room for them.
        let answer = unsafe {
            volca_lu_export(
                found.factors,
                block.as_mut_ptr().cast(),
                found.bytes as usize,
            )
        };
        assert_eq!(answer, DONE);
        // The row sums: the solution is one everywhere.
        let mut rhs: Vec<f64> = (0..n)
            .map(|i| (0..n).map(|j| vals[j * n + i]).sum())
            .collect();
        let mut scratch = vec![0.0; volca_lu_scratch_len(n, 1)];
        // SAFETY: the block just written, and two lists of this test's own.
        let answer = unsafe {
            volca_lu_solve(
                block.as_ptr().cast(),
                found.bytes as usize,
                rhs.as_mut_ptr(),
                rhs.len(),
                scratch.as_mut_ptr(),
                scratch.len(),
            )
        };
        assert_eq!(answer, DONE);
        assert!(rhs.iter().all(|x| (x - 1.0).abs() < 1e-12), "{rhs:?}");
    }

    #[test]
    fn a_refusal_comes_with_its_column_and_nothing_to_free() {
        let (rows, cols, vals) = ([0u32, 1, 0, 1], [0u32, 1, 1, 0], [1.0, 1.0, -1.0, -1.0]);
        let mut found = EMPTY;
        // SAFETY: three lists of one length, and a `Found` of this test's own.
        let answer = unsafe {
            volca_lu_factorize(
                2,
                4,
                rows.as_ptr(),
                cols.as_ptr(),
                vals.as_ptr(),
                &mut found,
            )
        };
        assert_eq!(answer, TINY_PIVOT);
        assert!(found.factors.is_null());
        assert!(found.at < 2);
        assert_eq!(found.value, 0.0);
    }

    #[test]
    fn factors_nobody_wants_are_freed() {
        let mut found = EMPTY;
        // SAFETY: three lists of one item, and a `Found` of this test's own.
        let answer = unsafe {
            volca_lu_factorize(1, 1, [0].as_ptr(), [0].as_ptr(), [2.0].as_ptr(), &mut found)
        };
        assert_eq!(answer, DONE);
        // SAFETY: the factors just received, given once.
        unsafe { volca_lu_discard(found.factors) };
    }

    #[test]
    fn what_is_not_a_block_is_refused() {
        let mut rhs = [1.0];
        let mut scratch = [0.0];
        let not_a_block = [0u64; 6];
        // SAFETY: three lists of this test's own, with their lengths.
        let answer = unsafe {
            volca_lu_solve(
                not_a_block.as_ptr().cast(),
                48,
                rhs.as_mut_ptr(),
                1,
                scratch.as_mut_ptr(),
                1,
            )
        };
        assert_eq!(answer, BAD_BLOCK);
    }

    /// The one-by-one matrix holding 2, factorized and exported.
    fn block_of_two() -> Vec<u64> {
        let mut found = EMPTY;
        // SAFETY: three lists of one item, and a `Found` of this test's own.
        let answer = unsafe {
            volca_lu_factorize(1, 1, [0].as_ptr(), [0].as_ptr(), [2.0].as_ptr(), &mut found)
        };
        assert_eq!(answer, DONE);
        let mut block = vec![0u64; found.bytes as usize / 8];
        // SAFETY: the factors just received, given once, and room for them.
        let answer = unsafe {
            volca_lu_export(
                found.factors,
                block.as_mut_ptr().cast(),
                found.bytes as usize,
            )
        };
        assert_eq!(answer, DONE);
        block
    }

    #[test]
    fn numbers_with_no_address_are_not_taken_for_none() {
        let block = block_of_two();
        let mut scratch = vec![0.0; volca_lu_scratch_len(1, 1)];
        let mut numbers = [4.0f64, 4.0];
        let between_two = numbers.as_mut_ptr().wrapping_byte_add(1);
        for rhs in [std::ptr::null_mut(), between_two] {
            // SAFETY: a block and working space of this test's own, and one
            // number announced where the function may read none.
            let answer = unsafe {
                volca_lu_solve(
                    block.as_ptr().cast(),
                    block.len() * 8,
                    rhs,
                    1,
                    scratch.as_mut_ptr(),
                    scratch.len(),
                )
            };
            assert_eq!(answer, BAD_SHAPE);
        }
        assert_eq!(numbers, [4.0, 4.0]);
    }

    /// Whether the processor holds the upper halves of its wide vector
    /// registers as in use, where it can say.
    #[cfg(target_arch = "x86_64")]
    fn wide_registers_in_use() -> Option<bool> {
        use std::arch::x86_64::__cpuid_count;
        if !std::arch::is_x86_feature_detected!("xsave") {
            return None;
        }
        // Whether the register read below takes the question asked of it.
        // SAFETY: every 64-bit processor answers this instruction.
        #[allow(unused_unsafe)]
        let asks = unsafe { __cpuid_count(0xd, 1) }.eax & 0b100 != 0;
        // SAFETY: the processor has the instruction, and takes this question.
        asks.then(|| unsafe { xgetbv_one() } & 0b100 != 0)
    }

    /// # Safety
    ///
    /// The processor reads this register with this argument.
    #[cfg(target_arch = "x86_64")]
    #[target_feature(enable = "xsave")]
    unsafe fn xgetbv_one() -> u64 {
        // SAFETY: the caller's promise.
        unsafe { std::arch::x86_64::_xgetbv(1) }
    }

    /// The full block is eliminated with wide vector instructions. Left marked
    /// as in use, their registers slow every later floating-point operation of
    /// the thread, the caller's included.
    #[cfg(target_arch = "x86_64")]
    #[test]
    fn a_factorization_leaves_the_wide_registers_released() {
        let n = 300;
        let (rows, cols, vals) = full(n as u32);
        let mut found = EMPTY;
        // SAFETY: three lists of one length, and a `Found` of this test's own.
        let answer = unsafe {
            volca_lu_factorize(
                n,
                vals.len(),
                rows.as_ptr(),
                cols.as_ptr(),
                vals.as_ptr(),
                &mut found,
            )
        };
        let in_use = wide_registers_in_use();
        // SAFETY: the factors just received, given once.
        unsafe { volca_lu_discard(found.factors) };
        assert_eq!(answer, DONE);
        assert_ne!(in_use, Some(true));
    }
}
