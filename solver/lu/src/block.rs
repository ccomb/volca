//! The factors, owned or read back from one flat block of bytes that another
//! allocator holds.
use crate::Failure;
use crate::factor::Raw;
use bytemuck::Pod;

/// Marks a block as written by this layout of the factors.
const MAGIC: u64 = 0x564f_4c43_414c_5531;

/// Finished factors.
pub struct Factors {
    raw: Raw,
    to_new: Vec<u32>,
    bytes: usize,
}

/// Factors as slices, wherever they live.
#[derive(Clone, Copy)]
pub struct View<'a> {
    pub(crate) lp: &'a [u64],
    pub(crate) up: &'a [u64],
    pub(crate) diag: &'a [f64],
    pub(crate) lx: &'a [f64],
    pub(crate) ux: &'a [f64],
    pub(crate) to_new: &'a [u32],
    pub(crate) li: &'a [u32],
    pub(crate) ui: &'a [u32],
}

impl Factors {
    pub(crate) fn new(raw: Raw, to_new: Vec<u32>) -> Result<Factors, Failure> {
        let bytes = byte_len(to_new.len(), raw.li.len(), raw.ui.len())?;
        Ok(Factors { raw, to_new, bytes })
    }

    pub fn view(&self) -> View<'_> {
        let r = &self.raw;
        View {
            lp: &r.lp,
            up: &r.up,
            diag: &r.diag,
            lx: &r.lx,
            ux: &r.ux,
            to_new: &self.to_new,
            li: &r.li,
            ui: &r.ui,
        }
    }

    /// Entries held, the diagonal included.
    pub fn entries(&self) -> usize {
        self.raw.li.len() + self.raw.ui.len() + self.raw.diag.len()
    }

    /// Size of the block `write` fills.
    pub fn byte_len(&self) -> usize {
        self.bytes
    }

    /// Copies the factors into `out`, which must be `byte_len` long. Eight-byte
    /// items come first so that a block starting on an eight-byte boundary has
    /// every item on its own boundary.
    pub fn write(&self, out: &mut [u8]) -> Result<(), Failure> {
        if out.len() != self.byte_len() {
            return Err(Failure::BadBlock);
        }
        let v = self.view();
        let head = [
            MAGIC,
            v.diag.len() as u64,
            v.li.len() as u64,
            v.ui.len() as u64,
        ];
        let mut rest = out;
        put(&mut rest, &head)?;
        put(&mut rest, v.lp)?;
        put(&mut rest, v.up)?;
        put(&mut rest, v.diag)?;
        put(&mut rest, v.lx)?;
        put(&mut rest, v.ux)?;
        put(&mut rest, v.to_new)?;
        put(&mut rest, v.li)?;
        put(&mut rest, v.ui)?;
        rest.fill(0);
        Ok(())
    }
}

impl<'a> View<'a> {
    /// Reads back a block `Factors::write` filled. Checks what a mistaken or
    /// truncated block gets wrong (mark, sizes, alignment), not every index.
    pub fn read(block: &'a [u8]) -> Result<View<'a>, Failure> {
        let mut rest = block;
        let &[magic, n, l, u] = take::<u64>(&mut rest, 4)? else {
            return Err(Failure::BadBlock);
        };
        let size = |x: u64| usize::try_from(x).map_err(|_| Failure::BadBlock);
        let (n, l, u) = (size(n)?, size(l)?, size(u)?);
        if magic != MAGIC || byte_len(n, l, u) != Ok(block.len()) {
            return Err(Failure::BadBlock);
        }
        let view = View {
            lp: take(&mut rest, n + 1)?,
            up: take(&mut rest, n + 1)?,
            diag: take(&mut rest, n)?,
            lx: take(&mut rest, l)?,
            ux: take(&mut rest, u)?,
            to_new: take(&mut rest, n)?,
            li: take(&mut rest, l)?,
            ui: take(&mut rest, u)?,
        };
        if view.lp[n] != l as u64 || view.up[n] != u as u64 {
            return Err(Failure::BadBlock);
        }
        Ok(view)
    }

    pub fn size(&self) -> usize {
        self.diag.len()
    }
}

fn byte_len(n: usize, l: usize, u: usize) -> Result<usize, Failure> {
    let sum = |a: usize, b: usize| a.checked_add(b).ok_or(Failure::TooLarge);
    let times = |a: usize, b: usize| a.checked_mul(b).ok_or(Failure::TooLarge);
    let items = sum(sum(n, l)?, u)?;
    let eights = sum(sum(4, times(2, sum(n, 1)?)?)?, items)?;
    let fours = sum(items, items % 2)?;
    sum(times(eights, 8)?, times(fours, 4)?)
}

fn put<T: Pod>(rest: &mut &mut [u8], items: &[T]) -> Result<(), Failure> {
    let bytes: &[u8] = bytemuck::cast_slice(items);
    let (now, later) = std::mem::take(rest)
        .split_at_mut_checked(bytes.len())
        .ok_or(Failure::BadBlock)?;
    now.copy_from_slice(bytes);
    *rest = later;
    Ok(())
}

fn take<'a, T: Pod>(rest: &mut &'a [u8], count: usize) -> Result<&'a [T], Failure> {
    let bytes = count
        .checked_mul(std::mem::size_of::<T>())
        .ok_or(Failure::BadBlock)?;
    let (now, later) = rest.split_at_checked(bytes).ok_or(Failure::BadBlock)?;
    *rest = later;
    bytemuck::try_cast_slice(now).map_err(|_| Failure::BadBlock)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Verdict, factorize};

    fn factors() -> Factors {
        let rows = [0, 1, 2, 1, 0, 2];
        let cols = [0, 1, 2, 0, 1, 1];
        let vals = [1.0, 1.0, 1.0, -0.5, -0.25, -0.125];
        match factorize(3, &rows, &cols, &vals).unwrap() {
            Verdict::Accepted(factors, _) => *factors,
            Verdict::Refused(refusal) => panic!("{refusal:?}"),
        }
    }

    /// Room for the block on an eight-byte boundary, and four bytes more.
    fn room(factors: &Factors) -> Vec<u64> {
        vec![0u64; factors.byte_len() / 8 + 1]
    }

    #[test]
    fn a_block_read_back_solves_like_the_factors_it_was_written_from() {
        let factors = factors();
        let mut space = room(&factors);
        let block = &mut bytemuck::cast_slice_mut(&mut space)[..factors.byte_len()];
        factors.write(block).unwrap();
        let (mut direct, mut through) = ([1.0, 2.0, 3.0], [1.0, 2.0, 3.0]);
        let mut scratch = [0.0; 3];
        factors.view().solve(&mut direct, &mut scratch).unwrap();
        View::read(block)
            .unwrap()
            .solve(&mut through, &mut scratch)
            .unwrap();
        assert_eq!(direct, through);
    }

    #[test]
    fn a_block_cut_short_is_refused() {
        let factors = factors();
        let mut space = room(&factors);
        let block = &mut bytemuck::cast_slice_mut(&mut space)[..factors.byte_len()];
        factors.write(block).unwrap();
        let short = &block[..block.len() - 8];
        assert_eq!(View::read(short).err(), Some(Failure::BadBlock));
        assert_eq!(View::read(&[]).err(), Some(Failure::BadBlock));
    }

    #[test]
    fn a_block_off_its_boundary_is_refused() {
        let factors = factors();
        let mut space = room(&factors);
        let block = &mut bytemuck::cast_slice_mut(&mut space)[4..4 + factors.byte_len()];
        factors.write(block).unwrap();
        assert_eq!(View::read(block).err(), Some(Failure::BadBlock));
    }

    #[test]
    fn a_block_of_another_size_is_not_written() {
        let factors = factors();
        let mut small = vec![0u8; factors.byte_len() - 1];
        assert_eq!(factors.write(&mut small).err(), Some(Failure::BadBlock));
    }
}
