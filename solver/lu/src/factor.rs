//! The factorization: `P M Pᵀ = L U`, one column at a time, each column
//! computed from the finished ones it depends on.
use crate::assemble::{Columns, columns};
use crate::block::Factors;
use crate::order::{Order, order};
use crate::{Failure, Original, Refusal, dense, filled, reserved, room};
use std::cmp::Ordering;

/// A pivot that keeps no more than this share of the diagonal entry it started
/// from is refused: the eliminations before it cancelled it out.
const TINY: f64 = 1e-12;

/// A trial solve leaves more than this share of a row unexplained: refused.
const UNRELIABLE: f64 = 1e-10;

/// When the rest of the matrix is eliminated as one full block: after a column
/// that took more than `ratio` steps for each cell the block would have, if
/// the block has at least `least` columns and fits in `most_bytes`.
#[derive(Clone, Copy)]
struct Cut {
    ratio: f64,
    least: usize,
    most_bytes: usize,
}

/// A column that costs this much is being filled in by nearly all the columns
/// left, and so will those after it. A block of fewer columns gains nothing,
/// and a larger one than this is not worth the memory it holds while it lasts.
const CUT: Cut = Cut {
    ratio: 1.0 / 120.0,
    least: 256,
    most_bytes: 200 << 20,
};

/// What a factorization did on its way.
#[derive(Clone, Copy, Debug)]
pub struct Report {
    /// Columns eliminated as a full block, zero when there was none.
    pub dense_columns: usize,
    /// Columns set first because eliminating them changes nothing else.
    pub free_columns: usize,
    /// Largest share of a row that the trial solve left unexplained.
    pub probe_error: f64,
}

pub enum Verdict {
    Accepted(Factors, Report),
    Refused(Refusal),
}

/// Factorizes the `n` by `n` matrix that holds `vals[k]` at row `rows[k]` and
/// column `cols[k]`; a position given several times holds the sum.
pub fn factorize(n: usize, rows: &[u32], cols: &[u32], vals: &[f64]) -> Result<Verdict, Failure> {
    factorize_cut(n, rows, cols, vals, CUT)
}

fn factorize_cut(
    n: usize,
    rows: &[u32],
    cols: &[u32],
    vals: &[f64],
    cut: Cut,
) -> Result<Verdict, Failure> {
    let m = columns(n, rows, cols, vals)?;
    let order = order(&m)?;
    let (b, floor) = reordered(&m, &order)?;
    let (raw, dense_columns) = match eliminate(&b, &floor, cut) {
        Ok(done) => done,
        Err(Stop::Failed(failure)) => return Err(failure),
        Err(Stop::Tiny { at, pivot }) => {
            let column = order.old_at(at);
            return Ok(Verdict::Refused(Refusal::TinyPivot { column, pivot }));
        }
    };
    let free_columns = order.free;
    let factors = Factors::new(raw, order.into_new())?;
    let probe_error = probe(&m, &factors)?;
    if probe_error > UNRELIABLE {
        let error = probe_error;
        return Ok(Verdict::Refused(Refusal::Unreliable { error }));
    }
    let report = Report {
        dense_columns,
        free_columns,
        probe_error,
    };
    Ok(Verdict::Accepted(factors, report))
}

/// The matrix with rows and columns in elimination order, and for each column
/// the smallest pivot it may end with.
fn reordered(m: &Columns, order: &Order) -> Result<(Columns, Vec<f64>), Failure> {
    let n = m.size();
    let mut ptr = reserved(n + 1)?;
    let mut row = reserved(m.row.len())?;
    let mut val = reserved(m.val.len())?;
    let mut floor = filled(n, 0f64)?;
    ptr.push(0);
    for (j, floor) in floor.iter_mut().enumerate() {
        let Original(old) = order.old_at(j);
        for (i, v) in m.column(old) {
            let i = order.new_of(Original(i));
            row.push(i as u32);
            val.push(v);
            if i == j {
                *floor = TINY * v.abs();
            }
        }
        ptr.push(row.len());
    }
    Ok((Columns { ptr, row, val }, floor))
}

/// `L` without its diagonal of ones, `U` without its diagonal, and that
/// diagonal, all by columns in elimination order.
pub(crate) struct Raw {
    pub lp: Vec<u64>,
    pub li: Vec<u32>,
    pub lx: Vec<f64>,
    pub up: Vec<u64>,
    pub ui: Vec<u32>,
    pub ux: Vec<f64>,
    pub diag: Vec<f64>,
}

enum Stop {
    Tiny { at: usize, pivot: f64 },
    Failed(Failure),
}

impl From<Failure> for Stop {
    fn from(failure: Failure) -> Self {
        Stop::Failed(failure)
    }
}

pub(crate) fn too_small(pivot: f64, floor: f64) -> bool {
    pivot.is_nan() || pivot.abs() <= floor
}

/// The last columns, set aside to be eliminated as one full block: what the
/// earlier columns leave of them goes above the block by columns (`top_*`) and
/// into the block itself.
struct Tail {
    first: usize,
    size: usize,
    block: Vec<f64>,
    top_ptr: Vec<usize>,
    top_idx: Vec<u32>,
    top_val: Vec<f64>,
}

/// The factors and how many columns went through a full block.
fn eliminate(b: &Columns, floor: &[f64], cut: Cut) -> Result<(Raw, usize), Stop> {
    let n = b.size();
    let mut search = Search::new(n)?;
    let mut x = filled(n, 0f64)?;
    let mut f = Raw {
        lp: reserved(n + 1)?,
        li: Vec::new(),
        lx: Vec::new(),
        up: reserved(n + 1)?,
        ui: Vec::new(),
        ux: Vec::new(),
        diag: filled(n, 0f64)?,
    };
    f.lp.push(0);
    f.up.push(0);
    let mut tail: Option<Tail> = None;
    for j in 0..n {
        let limit = tail.as_ref().map_or(j, |t| t.first);
        let mut top = n;
        for (i, v) in b.column(j) {
            x[i] = v;
            if search.mark[i] != j {
                top = search.from(i, limit, j, &f.lp, &f.li, top);
            }
        }
        let reached = &search.reach[top..n];
        let mut work = 0u64;
        for &k in reached {
            let k = k as usize;
            if k < limit {
                let xk = x[k];
                let r = f.lp[k] as usize..f.lp[k + 1] as usize;
                work += r.len() as u64;
                for (&i, &l) in f.li[r.clone()].iter().zip(&f.lx[r]) {
                    x[i as usize] -= l * xk;
                }
            }
        }
        match &mut tail {
            None => {
                let pivot = x[j];
                if too_small(pivot, floor[j]) {
                    return Err(Stop::Tiny { at: j, pivot });
                }
                room(&mut f.li, reached.len())?;
                room(&mut f.lx, reached.len())?;
                room(&mut f.ui, reached.len())?;
                room(&mut f.ux, reached.len())?;
                for &i in reached {
                    let v = std::mem::take(&mut x[i as usize]);
                    if v != 0.0 {
                        match (i as usize).cmp(&j) {
                            Ordering::Less => {
                                f.ui.push(i);
                                f.ux.push(v);
                            }
                            Ordering::Greater => {
                                f.li.push(i);
                                f.lx.push(v / pivot);
                            }
                            Ordering::Equal => {}
                        }
                    }
                }
                f.diag[j] = pivot;
                f.lp.push(f.li.len() as u64);
                f.up.push(f.ui.len() as u64);
                let size = n - j - 1;
                if switches(cut, work, size) {
                    let mut top_ptr = reserved(size + 1)?;
                    top_ptr.push(0);
                    let cells = size.checked_mul(size).ok_or(Failure::TooLarge)?;
                    tail = Some(Tail {
                        first: j + 1,
                        size,
                        block: filled(cells, 0f64)?,
                        top_ptr,
                        top_idx: Vec::new(),
                        top_val: Vec::new(),
                    });
                }
            }
            Some(t) => {
                room(&mut t.top_idx, reached.len())?;
                room(&mut t.top_val, reached.len())?;
                let column = (j - t.first) * t.size;
                for &i in reached {
                    let v = std::mem::take(&mut x[i as usize]);
                    if (i as usize) >= t.first {
                        t.block[column + (i as usize - t.first)] = v;
                    } else if v != 0.0 {
                        t.top_idx.push(i);
                        t.top_val.push(v);
                    }
                }
                t.top_ptr.push(t.top_idx.len());
            }
        }
    }
    let dense_columns = tail.as_ref().map_or(0, |t| t.size);
    if let Some(mut t) = tail {
        dense::eliminate(&mut t.block, t.size, |c| floor[t.first + c]).map_err(|(c, pivot)| {
            let at = t.first + c;
            Stop::Tiny { at, pivot }
        })?;
        for c in 0..t.size {
            let above = t.top_ptr[c]..t.top_ptr[c + 1];
            let column = &t.block[c * t.size..(c + 1) * t.size];
            room(&mut f.ui, above.len() + c)?;
            room(&mut f.ux, above.len() + c)?;
            room(&mut f.li, t.size - c)?;
            room(&mut f.lx, t.size - c)?;
            f.ui.extend_from_slice(&t.top_idx[above.clone()]);
            f.ux.extend_from_slice(&t.top_val[above]);
            for (r, &v) in column.iter().enumerate() {
                if v != 0.0 {
                    match r.cmp(&c) {
                        Ordering::Less => {
                            f.ui.push((t.first + r) as u32);
                            f.ux.push(v);
                        }
                        Ordering::Greater => {
                            f.li.push((t.first + r) as u32);
                            f.lx.push(v);
                        }
                        Ordering::Equal => {}
                    }
                }
            }
            f.diag[t.first + c] = column[c];
            f.lp.push(f.li.len() as u64);
            f.up.push(f.ui.len() as u64);
        }
    }
    f.li.shrink_to_fit();
    f.lx.shrink_to_fit();
    f.ui.shrink_to_fit();
    f.ux.shrink_to_fit();
    Ok((f, dense_columns))
}

fn switches(cut: Cut, work: u64, size: usize) -> bool {
    let cells = size.saturating_mul(size);
    size >= cut.least
        && cells.saturating_mul(8) <= cut.most_bytes
        && work as f64 >= cut.ratio * cells as f64
}

/// Which finished columns a new column depends on, found by walking from each
/// of its entries through the multipliers of the columns already finished.
struct Search {
    mark: Vec<usize>,
    /// Filled from the end; read forward, every column comes after those it depends on.
    reach: Vec<u32>,
    stack: Vec<u32>,
    resume: Vec<usize>,
}

impl Search {
    fn new(n: usize) -> Result<Search, Failure> {
        Ok(Search {
            mark: filled(n, usize::MAX)?,
            reach: filled(n, 0)?,
            stack: filled(n, 0)?,
            resume: filled(n, 0)?,
        })
    }

    /// Adds `root` and all it reaches to `reach[top..]` and returns the new
    /// `top`. Columns from `limit` on are not finished: they are reached but
    /// not walked through.
    fn from(
        &mut self,
        root: usize,
        limit: usize,
        stamp: usize,
        lp: &[u64],
        li: &[u32],
        mut top: usize,
    ) -> usize {
        let start = |node: usize| if node < limit { lp[node] as usize } else { 0 };
        let mut depth = 0;
        self.stack[0] = root as u32;
        self.resume[0] = start(root);
        self.mark[root] = stamp;
        loop {
            let node = self.stack[depth] as usize;
            let end = if node < limit {
                lp[node + 1] as usize
            } else {
                0
            };
            let mut next = None;
            let mut p = self.resume[depth];
            while p < end {
                let i = li[p] as usize;
                p += 1;
                if self.mark[i] != stamp {
                    next = Some(i);
                    break;
                }
            }
            match next {
                Some(i) => {
                    self.mark[i] = stamp;
                    self.resume[depth] = p;
                    depth += 1;
                    self.stack[depth] = i as u32;
                    self.resume[depth] = start(i);
                }
                None => {
                    top -= 1;
                    self.reach[top] = node as u32;
                    if depth == 0 {
                        return top;
                    }
                    depth -= 1;
                }
            }
        }
    }
}

/// What the factors lose on a solve that uses every column: one unit of every
/// product is asked for, and the result is the largest share of any row's terms
/// that the computed solution leaves unexplained.
///
/// This judges the factorization and not the matrix: a matrix that is merely
/// sensitive to its own data scores as well as any other, a factorization that
/// went through a pivot far smaller than its neighbours does not.
fn probe(m: &Columns, factors: &Factors) -> Result<f64, Failure> {
    let n = m.size();
    let mut left = filled(n, 1f64)?;
    let mut x = filled(n, 1f64)?;
    let mut scratch = filled(n, 0f64)?;
    factors.view().solve(&mut x, &mut scratch)?;
    let mut terms = filled(n, 1f64)?;
    for (j, xj) in x.iter().enumerate() {
        for (i, v) in m.column(j) {
            left[i] -= v * xj;
            terms[i] += (v * xj).abs();
        }
    }
    let share = |(left, terms): (&f64, &f64)| {
        let share = left.abs() / terms;
        // A solution that overflowed explains nothing.
        if share.is_nan() { f64::INFINITY } else { share }
    };
    Ok(left.iter().zip(&terms).map(share).fold(0.0, f64::max))
}

#[cfg(test)]
mod tests {
    use super::*;

    const NO_BLOCK: Cut = Cut {
        ratio: 0.0,
        least: 1,
        most_bytes: 0,
    };

    /// Everything after the first column goes through the block.
    const ALL_BLOCK: Cut = Cut {
        ratio: 0.0,
        least: 1,
        most_bytes: usize::MAX,
    };

    type Entries = (Vec<u32>, Vec<u32>, Vec<f64>);

    fn entries(n: usize, entry: impl Fn(usize, usize) -> f64) -> Entries {
        let mut out = (Vec::new(), Vec::new(), Vec::new());
        for (i, j) in (0..n).flat_map(|j| (0..n).map(move |i| (i, j))) {
            if entry(i, j) != 0.0 {
                out.0.push(i as u32);
                out.1.push(j as u32);
                out.2.push(entry(i, j));
            }
        }
        out
    }

    fn accepted(n: usize, (rows, cols, vals): &Entries, cut: Cut) -> (Factors, Report) {
        match factorize_cut(n, rows, cols, vals, cut).unwrap() {
            Verdict::Accepted(factors, report) => (factors, report),
            Verdict::Refused(refusal) => panic!("{refusal:?}"),
        }
    }

    fn refused(n: usize, (rows, cols, vals): &Entries, cut: Cut) -> Refusal {
        match factorize_cut(n, rows, cols, vals, cut).unwrap() {
            Verdict::Accepted(..) => panic!("accepted"),
            Verdict::Refused(refusal) => refusal,
        }
    }

    fn solved(factors: &Factors, rhs: &[f64]) -> Vec<f64> {
        let n = factors.view().size();
        let mut x = rhs.to_vec();
        let mut scratch = vec![0.0; crate::scratch_len(n, rhs.len().checked_div(n).unwrap_or(0))];
        factors.view().solve(&mut x, &mut scratch).unwrap();
        x
    }

    /// The right-hand side that has `x` for solution.
    fn times(n: usize, entry: impl Fn(usize, usize) -> f64, x: &[f64]) -> Vec<f64> {
        (0..n)
            .map(|i| (0..n).map(|j| entry(i, j) * x[j]).sum())
            .collect()
    }

    fn furthest(a: &[f64], b: &[f64]) -> f64 {
        assert_eq!(a.len(), b.len());
        a.iter()
            .zip(b)
            .map(|(a, b)| (a - b).abs())
            .fold(0.0, f64::max)
    }

    /// One on the diagonal, and off it something small of either sign in about
    /// one position out of `one_in`.
    fn exchanges(one_in: usize) -> impl Fn(usize, usize) -> f64 {
        move |i, j| {
            let draw = (i * 7919 + j * 104_729 + 13) % 1009;
            match (i == j, draw % one_in == 0) {
                (true, _) => 1.0,
                (false, true) => draw as f64 / 20_000.0 - 0.025,
                (false, false) => 0.0,
            }
        }
    }

    #[test]
    fn a_matrix_without_columns_is_accepted_and_solves_nothing() {
        let (factors, _) = accepted(0, &entries(0, |_, _| 0.0), CUT);
        assert_eq!(solved(&factors, &[]), Vec::<f64>::new());
    }

    #[test]
    fn a_single_column_divides_by_its_diagonal() {
        let (factors, _) = accepted(1, &entries(1, |_, _| 4.0), CUT);
        assert_eq!(solved(&factors, &[2.0]), vec![0.5]);
    }

    #[test]
    fn a_known_solution_is_found_again() {
        let n = 60;
        let x: Vec<f64> = (0..n).map(|i| 1.0 + i as f64 / 7.0).collect();
        let rhs = times(n, exchanges(5), &x);
        let (factors, report) = accepted(n, &entries(n, exchanges(5)), CUT);
        assert_eq!(report.dense_columns, 0);
        assert!(furthest(&solved(&factors, &rhs), &x) < 1e-12);
    }

    #[test]
    fn a_position_given_twice_holds_the_sum() {
        let whole = accepted(
            2,
            &(vec![0, 1, 0], vec![0, 1, 1], vec![1.0, 1.0, -0.5]),
            CUT,
        )
        .0;
        let halves = (
            vec![0, 1, 0, 0],
            vec![0, 1, 1, 1],
            vec![1.0, 1.0, -0.25, -0.25],
        );
        let halves = accepted(2, &halves, CUT).0;
        assert_eq!(solved(&whole, &[1.0, 2.0]), solved(&halves, &[1.0, 2.0]));
    }

    #[test]
    fn a_triangular_matrix_is_all_free_columns_and_fills_nothing() {
        let n = 30;
        let below = |i: usize, j: usize| if i >= j { exchanges(2)(i, j) } else { 0.0 };
        let given = entries(n, below);
        let (factors, report) = accepted(n, &given, CUT);
        assert_eq!(report.free_columns, n);
        assert_eq!(factors.entries(), given.2.len());
        let x: Vec<f64> = (0..n).map(|i| i as f64 - 3.0).collect();
        assert!(furthest(&solved(&factors, &times(n, below, &x)), &x) < 1e-12);
    }

    #[test]
    fn the_block_gives_the_solution_the_columns_give() {
        let n = 80;
        let given = entries(n, exchanges(3));
        let (by_columns, none) = accepted(n, &given, NO_BLOCK);
        let (by_block, some) = accepted(n, &given, ALL_BLOCK);
        assert_eq!(none.dense_columns, 0);
        assert!(some.dense_columns > 0);
        let rhs: Vec<f64> = (0..n).map(|i| (i % 5) as f64).collect();
        let (a, b) = (solved(&by_columns, &rhs), solved(&by_block, &rhs));
        assert!(furthest(&a, &b) < 1e-12);
    }

    #[test]
    fn a_full_matrix_goes_through_the_block_by_itself() {
        let n = 300;
        let full = |i: usize, j: usize| {
            if i == j {
                1.0
            } else {
                exchanges(1)(i, j) / 20.0
            }
        };
        let (factors, report) = accepted(n, &entries(n, full), CUT);
        assert!(report.dense_columns >= CUT.least);
        assert!(report.probe_error < 1e-13);
        let x: Vec<f64> = (0..n).map(|i| (i % 9) as f64 - 4.0).collect();
        assert!(furthest(&solved(&factors, &times(n, full, &x)), &x) < 1e-11);
    }

    #[test]
    fn several_columns_at_once_are_solved_as_one_by_one() {
        let n = 40;
        let (factors, _) = accepted(n, &entries(n, exchanges(4)), CUT);
        let rhs: Vec<f64> = (0..11 * n).map(|p| ((p * 31) % 17) as f64 - 8.0).collect();
        let together = solved(&factors, &rhs);
        let apart: Vec<f64> = rhs
            .chunks(n)
            .flat_map(|one| solved(&factors, one))
            .collect();
        assert_eq!(together, apart);
    }

    /// 0 and 3 stand alone, 1 and 2 each take one unit of the other to make one.
    fn two_that_feed_each_other_whole(i: usize, j: usize) -> f64 {
        match (i, j) {
            (1, 2) | (2, 1) => -1.0,
            _ if i == j => 1.0,
            _ => 0.0,
        }
    }

    #[test]
    fn two_columns_that_cancel_each_other_are_refused_and_one_is_named() {
        let given = entries(4, two_that_feed_each_other_whole);
        for cut in [NO_BLOCK, ALL_BLOCK] {
            match refused(4, &given, cut) {
                Refusal::TinyPivot { column, pivot } => {
                    assert!(column == Original(1) || column == Original(2), "{column:?}");
                    assert_eq!(pivot, 0.0);
                }
                other => panic!("{other:?}"),
            }
        }
    }

    #[test]
    fn factors_that_lose_a_known_solution_are_refused() {
        // Whichever column goes first, its pivot is its own tiny diagonal: no
        // elimination cancelled it, and it still costs eleven of sixteen digits.
        let weak = |i: usize, j: usize| if i == j { 1e-11 } else { 1.0 };
        match refused(2, &entries(2, weak), CUT) {
            Refusal::Unreliable { error } => assert!(error > UNRELIABLE),
            other => panic!("{other:?}"),
        }
    }

    #[test]
    fn a_size_that_does_not_go_with_the_entries_is_a_failure_and_not_a_verdict() {
        assert_eq!(
            factorize(1, &[0], &[1], &[1.0]).err(),
            Some(Failure::BadEntry(0))
        );
        assert_eq!(
            factorize(1, &[0], &[], &[1.0]).err(),
            Some(Failure::BadShape)
        );
    }
}
