//! The solver as the engine links it: a static archive with a C face.
//!
//! This is the only crate of the workspace where `unsafe` may appear, because
//! a symbol the linker sees and a pointer received from another language cannot
//! be written without it.
use lu::Original;
use std::panic::{catch_unwind, resume_unwind};

const SOUND: u32 = 0;
const WRONG_ORDER: u32 = 1;
const UNWINDING_WENT_THROUGH: u32 = 2;
const PANICKED: u32 = 3;

/// Says whether this archive works once linked into a foreign program, which a
/// build that merely succeeds does not show: the code of a dependency runs,
/// the allocator answers, and a panic stops here instead of killing the caller.
/// Answers 0 when all three hold.
// SAFETY: the name is prefixed, and nothing else the engine links defines it.
#[unsafe(no_mangle)]
pub extern "C" fn volca_solver_probe() -> u32 {
    catch_unwind(probe).unwrap_or(PANICKED)
}

fn probe() -> u32 {
    // 0 and 1 feed each other, 2 uses 1 and nobody uses 2, 3 feeds 0 and uses
    // nobody: 2 and 3 go first, and 0 and 1 are left for the dependency to order.
    let rows = [0, 1, 2, 3, 1, 0, 1, 3];
    let cols = [0, 1, 2, 3, 0, 1, 2, 0];
    let vals = [1.0, 1.0, 1.0, 1.0, -0.5, -0.5, -0.5, -0.5];
    let expected = lu::elimination_order(4, &rows, &cols, &vals).is_ok_and(|order| {
        order.free == 2 && order.new_of(Original(2)) == 0 && order.new_of(Original(3)) == 1
    });
    if !expected {
        return WRONG_ORDER;
    }
    // Unwinds like a panic, without the message a panic prints.
    match catch_unwind(|| resume_unwind(Box::new(()))) {
        Err(_) => SOUND,
        Ok(()) => UNWINDING_WENT_THROUGH,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_probe_is_sound_where_it_was_built() {
        assert_eq!(volca_solver_probe(), SOUND);
    }
}
