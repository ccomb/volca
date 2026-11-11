//! Elimination of a full square block, where nearly every entry is in use and
//! working column by column over index lists would waste the processor.
use crate::factor::too_small;
use faer::linalg::matmul::matmul;
use faer::linalg::triangular_solve as triangular;
use faer::reborrow::*;
use faer::{Accum, MatMut, Par};

/// Columns eliminated together before the rest of the block is updated in one
/// matrix product.
const PANEL: usize = 96;

/// Factors the block in place, pivots kept on the diagonal: afterwards the
/// multipliers sit below the diagonal and the eliminated rows on and above it.
/// Stops at the first pivot smaller than `floor` says its column allows, and
/// returns that column and pivot.
///
/// A position that no product ever reaches stays exactly zero, which is what
/// lets the caller store the result as a sparse matrix again.
pub(crate) fn eliminate(
    block: &mut [f64],
    size: usize,
    floor: impl Fn(usize) -> f64,
) -> Result<(), (usize, f64)> {
    let mut a = MatMut::from_column_major_slice_mut(block, size, size);
    let mut done = 0;
    while done < size {
        let width = PANEL.min(size - done);
        let rest = size - done;
        let (mut corner, mut right, mut below, trailing) = a
            .rb_mut()
            .submatrix_mut(done, done, rest, rest)
            .split_at_mut(width, width);
        for p in 0..width {
            let pivot = corner[(p, p)];
            if too_small(pivot, floor(done + p)) {
                return Err((done + p, pivot));
            }
            for i in p + 1..width {
                corner[(i, p)] /= pivot;
            }
            for j in p + 1..width {
                let u = corner[(p, j)];
                if u != 0.0 {
                    for i in p + 1..width {
                        let l = corner[(i, p)];
                        corner[(i, j)] -= l * u;
                    }
                }
            }
        }
        if width < rest {
            triangular::solve_unit_lower_triangular_in_place(corner.rb(), right.rb_mut(), Par::Seq);
            triangular::solve_lower_triangular_in_place(
                corner.rb().transpose(),
                below.rb_mut().transpose_mut(),
                Par::Seq,
            );
            matmul(trailing, Accum::Add, below.rb(), right.rb(), -1.0, Par::Seq);
            release_wide_registers();
        }
        done += width;
    }
    Ok(())
}

/// The products above leave the wide vector registers marked as in use, and
/// the processor then makes every ordinary floating-point operation of the
/// thread wait on them: solving ran two to three times slower on a thread that
/// had factorized, for as long as it lived, and so did the caller's own code.
#[cfg(target_arch = "x86_64")]
fn release_wide_registers() {
    if let Some(simd) = faer::traits::pulp::x86::V3::try_new() {
        simd.avx._mm256_zeroupper();
    }
}

#[cfg(not(target_arch = "x86_64"))]
fn release_wide_registers() {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn factors_multiply_back_to_the_block() {
        let n = 150;
        let entry = |i: usize, j: usize| {
            if i == j {
                1.0
            } else {
                ((i * 7 + j * 13) % 11) as f64 / 400.0 - 0.01
            }
        };
        let mut block: Vec<f64> = (0..n * n).map(|p| entry(p % n, p / n)).collect();
        eliminate(&mut block, n, |_| 1e-12).unwrap();
        let lower = |i: usize, k: usize| if k == i { 1.0 } else { block[k * n + i] };
        let mut worst = 0.0f64;
        for j in 0..n {
            for i in 0..n {
                let product: f64 = (0..=i.min(j)).map(|k| lower(i, k) * block[j * n + k]).sum();
                worst = worst.max((product - entry(i, j)).abs());
            }
        }
        assert!(worst < 1e-12, "{worst}");
    }

    #[test]
    fn a_vanishing_pivot_is_named() {
        let mut block = vec![1.0, 1.0, 1.0, 1.0];
        assert_eq!(eliminate(&mut block, 2, |_| 1e-12).err(), Some((1, 0.0)));
    }
}
