//! The order in which columns are eliminated. It decides how many entries the
//! factors hold, and nothing else: any order gives the same solution.
use crate::assemble::Columns;
use crate::{Failure, Original, filled, reserved};
use faer::dyn_stack::{MemBuffer, MemStack};
use faer::sparse::SymbolicSparseColMatRef;
use faer::sparse::linalg::amd;

/// Where each row and column goes. The two directions are kept apart by type:
/// `new_of` takes the caller's numbering, `old_at` a position in the factors.
pub struct Order {
    to_old: Vec<u32>,
    to_new: Vec<u32>,
    /// How many columns, placed first, change no other when eliminated.
    pub free: usize,
}

impl Order {
    pub fn old_at(&self, position: usize) -> Original {
        Original(self.to_old[position] as usize)
    }

    pub fn new_of(&self, Original(i): Original) -> usize {
        self.to_new[i] as usize
    }
}

const PLACED: u32 = u32::MAX;

/// Columns nobody else needs first, then columns that need nobody else, then
/// the rest by minimum degree.
///
/// A column whose row is otherwise empty changes no later column when it is
/// eliminated, and neither does a column that is otherwise empty itself. Taking
/// them out one after the other, each in the order that keeps this true for the
/// next, leaves the knot of columns that feed one another: the only part where
/// elimination creates entries.
pub(crate) fn order(m: &Columns) -> Result<Order, Failure> {
    let n = m.size();
    let rows = by_rows(m)?;
    let mut to_old = reserved(n)?;
    let mut waiting = filled(n, 0u32)?;
    // Nobody left uses it: its users are placed, its suppliers lose a user.
    for (i, users) in waiting.iter_mut().enumerate() {
        *users = rows.row(i).filter(|&j| j != i).count() as u32;
    }
    take_free(&mut waiting, &mut to_old, |j| m.column(j).map(|(i, _)| i));
    // It uses nobody left: its suppliers are placed, its users lose a supplier.
    for (j, suppliers) in waiting.iter_mut().enumerate() {
        if *suppliers != PLACED {
            *suppliers = m.column(j).filter(|&(i, _)| i != j).count() as u32;
        }
    }
    take_free(&mut waiting, &mut to_old, |i| rows.row(i));
    let free = to_old.len();
    place_knot(m, &waiting, &mut to_old)?;
    let mut to_new = filled(n, 0u32)?;
    for (position, &old) in to_old.iter().enumerate() {
        to_new[old as usize] = position as u32;
    }
    Ok(Order {
        to_old,
        to_new,
        free,
    })
}

/// Places every node nothing is waiting on, then those this frees, and so on.
fn take_free<I: Iterator<Item = usize>>(
    waiting: &mut [u32],
    placed: &mut Vec<u32>,
    freed_by: impl Fn(usize) -> I,
) {
    let start = placed.len();
    for (i, w) in waiting.iter_mut().enumerate() {
        if *w == 0 {
            *w = PLACED;
            placed.push(i as u32);
        }
    }
    let mut at = start;
    while at < placed.len() {
        let done = placed[at] as usize;
        at += 1;
        for other in freed_by(done) {
            if waiting[other] != PLACED {
                waiting[other] -= 1;
                if waiting[other] == 0 {
                    waiting[other] = PLACED;
                    placed.push(other as u32);
                }
            }
        }
    }
}

/// Who sits in each row: row `i` holds the columns `col[ptr[i]..ptr[i + 1]]`.
struct Rows {
    ptr: Vec<usize>,
    col: Vec<u32>,
}

impl Rows {
    fn row(&self, i: usize) -> impl Iterator<Item = usize> + '_ {
        self.col[self.ptr[i]..self.ptr[i + 1]]
            .iter()
            .map(|&j| j as usize)
    }
}

fn by_rows(m: &Columns) -> Result<Rows, Failure> {
    let n = m.size();
    let mut ptr = filled(n + 1, 0usize)?;
    for &i in &m.row {
        ptr[i as usize + 1] += 1;
    }
    for i in 0..n {
        ptr[i + 1] += ptr[i];
    }
    let mut next = filled(n, 0usize)?;
    next.copy_from_slice(&ptr[..n]);
    let mut col = filled(m.row.len(), 0u32)?;
    for j in 0..n {
        for (i, _) in m.column(j) {
            col[next[i]] = j as u32;
            next[i] += 1;
        }
    }
    Ok(Rows { ptr, col })
}

/// Appends the nodes still waiting, in minimum degree order on the pattern of
/// the matrix plus its transpose restricted to them.
fn place_knot(m: &Columns, waiting: &[u32], placed: &mut Vec<u32>) -> Result<(), Failure> {
    let n = m.size();
    let inside = |i: usize| waiting[i] != PLACED;
    let mut members = reserved(n - placed.len())?;
    members.extend((0..n).filter(|&i| inside(i)).map(|i| i as u32));
    let size = members.len();
    let mut local = filled(n, 0usize)?;
    for (k, &i) in members.iter().enumerate() {
        local[i as usize] = k;
    }
    let mut ptr = filled(size + 1, 0usize)?;
    let mut idx = reserved(m.row.len())?;
    for (k, &j) in members.iter().enumerate() {
        let j = j as usize;
        idx.extend(
            m.column(j)
                .filter(|&(i, _)| i != j && inside(i))
                .map(|(i, _)| local[i]),
        );
        ptr[k + 1] = idx.len();
    }
    let mut perm = filled(size, 0usize)?;
    let mut inverse = filled(size, 0usize)?;
    let pattern = SymbolicSparseColMatRef::new_unsorted_checked(size, size, &ptr, None, &idx);
    let scratch = amd::order_maybe_unsorted_scratch::<usize>(size, idx.len());
    let mut memory = MemBuffer::try_new(scratch).map_err(|_| Failure::OutOfMemory)?;
    amd::order_maybe_unsorted(
        &mut perm,
        &mut inverse,
        pattern,
        amd::Control::default(),
        MemStack::new(&mut memory),
    )
    .map_err(|_| Failure::TooLarge)?;
    placed.extend(perm.iter().map(|&k| members[k]));
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assemble::columns;

    /// 0 and 1 feed each other, 2 uses 1 and nobody uses 2, 3 feeds 0 and uses nobody.
    #[test]
    fn free_columns_come_first_and_the_knot_last() {
        let rows = [0, 1, 2, 3, 1, 0, 1, 3];
        let cols = [0, 1, 2, 3, 0, 1, 2, 0];
        let vals = [1.0, 1.0, 1.0, 1.0, -0.5, -0.5, -0.5, -0.5];
        let o = order(&columns(4, &rows, &cols, &vals).unwrap()).unwrap();
        assert_eq!(o.free, 2);
        assert_eq!(o.new_of(Original(2)), 0);
        assert_eq!(o.new_of(Original(3)), 1);
        let round_trip: Vec<_> = (0..4).map(|p| o.new_of(o.old_at(p))).collect();
        assert_eq!(round_trip, vec![0, 1, 2, 3]);
    }

    #[test]
    fn an_empty_matrix_has_an_empty_order() {
        assert_eq!(order(&columns(0, &[], &[], &[]).unwrap()).unwrap().free, 0);
    }
}
