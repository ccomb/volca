//! Solving with finished factors. Nothing here writes to the factors, so any
//! number of threads may solve with the same ones at once, and nothing here
//! asks for memory: the caller brings the working space.
use crate::Failure;
use crate::block::View;

/// How many columns are solved side by side at most.
const WIDEST: usize = 8;

/// How many columns wide the working space is for a group of this many.
fn lanes(columns: usize) -> usize {
    match columns {
        0 | 1 => 1,
        2..=4 => 4,
        _ => WIDEST,
    }
}

/// How many numbers of working space solving `columns` columns takes, for a
/// matrix `n` wide.
pub fn scratch_len(n: usize, columns: usize) -> usize {
    n.saturating_mul(lanes(columns))
}

impl View<'_> {
    /// Replaces each column of `rhs` (one after the other, each as long as the
    /// matrix is wide) by the solution for it. `scratch` is working space of at
    /// least `scratch_len` numbers, whose content before and after means nothing.
    ///
    /// Columns are solved several at a time: the factors are read once for the
    /// group, and the arithmetic for the group fits the processor's vector
    /// registers.
    pub fn solve(&self, rhs: &mut [f64], scratch: &mut [f64]) -> Result<(), Failure> {
        let n = self.size();
        if n == 0 {
            return if rhs.is_empty() {
                Ok(())
            } else {
                Err(Failure::BadShape)
            };
        }
        if !rhs.len().is_multiple_of(n) || scratch.len() < scratch_len(n, rhs.len() / n) {
            return Err(Failure::BadShape);
        }
        let mut rest = rhs;
        while !rest.is_empty() {
            let columns = (rest.len() / n).min(WIDEST);
            let (now, later) = std::mem::take(&mut rest).split_at_mut(columns * n);
            match lanes(columns) {
                1 => self.sweep::<1>(now, scratch)?,
                4 => self.sweep::<4>(now, scratch)?,
                _ => self.sweep::<WIDEST>(now, scratch)?,
            }
            rest = later;
        }
        Ok(())
    }

    /// Solves up to `W` columns side by side.
    fn sweep<const W: usize>(&self, columns: &mut [f64], scratch: &mut [f64]) -> Result<(), Failure>
    where
        [f64; W]: bytemuck::Pod,
    {
        let n = self.size();
        let space = scratch.get_mut(..n * W).ok_or(Failure::BadShape)?;
        let y: &mut [[f64; W]] =
            bytemuck::try_cast_slice_mut(space).map_err(|_| Failure::BadShape)?;
        y.fill([0.0; W]);
        for (c, column) in columns.chunks_exact(n).enumerate() {
            for (&v, &to) in column.iter().zip(self.to_new) {
                y[to as usize][c] = v;
            }
        }
        for k in 0..n {
            let yk = y[k];
            if yk != [0.0; W] {
                let r = self.lp[k] as usize..self.lp[k + 1] as usize;
                for (&i, &l) in self.li[r.clone()].iter().zip(&self.lx[r]) {
                    for (target, source) in y[i as usize].iter_mut().zip(&yk) {
                        *target -= l * source;
                    }
                }
            }
        }
        for j in (0..n).rev() {
            if y[j] != [0.0; W] {
                let pivot = self.diag[j];
                for value in &mut y[j] {
                    *value /= pivot;
                }
                let yj = y[j];
                let r = self.up[j] as usize..self.up[j + 1] as usize;
                for (&i, &u) in self.ui[r.clone()].iter().zip(&self.ux[r]) {
                    for (target, source) in y[i as usize].iter_mut().zip(&yj) {
                        *target -= u * source;
                    }
                }
            }
        }
        for (c, column) in columns.chunks_exact_mut(n).enumerate() {
            for (v, &to) in column.iter_mut().zip(self.to_new) {
                *v = y[to as usize][c];
            }
        }
        Ok(())
    }
}
