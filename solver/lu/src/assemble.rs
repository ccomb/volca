//! From a list of entries to a matrix by columns.
use crate::{Failure, filled};

/// A square matrix by columns: column `j` holds `row[ptr[j]..ptr[j + 1]]`.
pub(crate) struct Columns {
    pub ptr: Vec<usize>,
    pub row: Vec<u32>,
    pub val: Vec<f64>,
}

impl Columns {
    pub fn size(&self) -> usize {
        self.ptr.len() - 1
    }

    pub fn column(&self, j: usize) -> impl Iterator<Item = (usize, f64)> + '_ {
        let r = self.ptr[j]..self.ptr[j + 1];
        self.row[r.clone()]
            .iter()
            .zip(&self.val[r])
            .map(|(&i, &v)| (i as usize, v))
    }
}

/// The matrix by columns, a position given several times holding the sum.
pub(crate) fn columns(
    n: usize,
    rows: &[u32],
    cols: &[u32],
    vals: &[f64],
) -> Result<Columns, Failure> {
    if rows.len() != cols.len() || rows.len() != vals.len() {
        return Err(Failure::BadShape);
    }
    if u32::try_from(n).is_err() {
        return Err(Failure::TooLarge);
    }
    let mut ptr = filled(n + 1, 0usize)?;
    for (at, ((&i, &j), v)) in rows.iter().zip(cols).zip(vals).enumerate() {
        if i as usize >= n || j as usize >= n || !v.is_finite() {
            return Err(Failure::BadEntry(at));
        }
        ptr[j as usize + 1] += 1;
    }
    for j in 0..n {
        ptr[j + 1] += ptr[j];
    }
    let mut next = filled(n, 0usize)?;
    next.copy_from_slice(&ptr[..n]);
    let mut row = filled(rows.len(), 0u32)?;
    let mut val = filled(rows.len(), 0f64)?;
    for ((&i, &j), &v) in rows.iter().zip(cols).zip(vals) {
        let p = next[j as usize];
        row[p] = i;
        val[p] = v;
        next[j as usize] = p + 1;
    }
    // Compact in place: `slot[i]` is one past where row `i` sits, and only
    // counts when it lies inside the column being written.
    let mut slot = next;
    slot.fill(0);
    let (mut out, mut from) = (0, 0);
    for j in 0..n {
        let (begin, to) = (out, ptr[j + 1]);
        for p in from..to {
            let i = row[p] as usize;
            if slot[i] > begin {
                val[slot[i] - 1] += val[p];
            } else {
                row[out] = row[p];
                val[out] = val[p];
                out += 1;
                slot[i] = out;
            }
        }
        ptr[j] = begin;
        from = to;
    }
    ptr[n] = out;
    row.truncate(out);
    val.truncate(out);
    Ok(Columns { ptr, row, val })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn repeated_positions_add_up() {
        let m = columns(
            2,
            &[0, 1, 0, 1, 0],
            &[0, 0, 1, 1, 0],
            &[1.0, 2.0, 3.0, 4.0, 0.5],
        )
        .unwrap();
        assert_eq!(m.column(0).collect::<Vec<_>>(), vec![(0, 1.5), (1, 2.0)]);
        assert_eq!(m.column(1).collect::<Vec<_>>(), vec![(0, 3.0), (1, 4.0)]);
    }

    #[test]
    fn an_entry_outside_the_matrix_is_named() {
        assert_eq!(
            columns(2, &[0, 2], &[0, 0], &[1.0, 1.0]).err(),
            Some(Failure::BadEntry(1))
        );
        assert_eq!(
            columns(2, &[0], &[0], &[f64::NAN]).err(),
            Some(Failure::BadEntry(0))
        );
    }
}
