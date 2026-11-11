#!/bin/bash
# Generate cabal.project.local for building volca with the solver's Rust
# archive.
# Shared between build.sh and Dockerfile.
#
# Optional env vars:
#   LINK_MODE               "dynamic" (default), "musl", "darwin", "windows"
#   OUTPUT_DIR              Where to write cabal.project.local (default: current dir)
#   VOLCA_OPT_LEVEL         0, 1 or 2 (default 2) - see the block below
#
# Output: writes cabal.project.local in OUTPUT_DIR, and
# solver/target/archive.project beside this script

set -e

LINK_MODE="${LINK_MODE:-dynamic}"
OUTPUT="${OUTPUT_DIR:-.}/cabal.project.local"

# The archive `cargo build --release` leaves under solver/, which volca.cabal
# names in extra-libraries. Absolute, because cabal reads a relative
# extra-lib-dirs from each package's own directory, and the projects that
# build the engine as a dependency of theirs sit somewhere else.
#
# Each mode below says which system libraries the archive needs named on the
# link line. `cargo rustc --release -p ffi --lib -- --print native-static-libs`
# lists what it asks for on a platform; the linker brings some of them itself.
SOLVER_TARGET_DIR="$(cd "$(dirname "$0")" && pwd)/solver/target"
SOLVER_LIB_DIR="$SOLVER_TARGET_DIR/release"

# Optimization level for volca's own code, as a per-package override of the
# global `optimization: 2` (which must stay 2 so the prebuilt cabal store's
# deps keep matching). Deps stay -O2 whatever this is, which is what makes the
# knob cheap: only volca's own modules move.
#
# The level follows what the build is for, and each caller picks its own:
#   0  anything that only has to run the suite: the CI test rows, and
#      `build.sh --test` from a working copy
#   1  a working-copy binary that is going to be run (`build.sh` with no
#      --test), and a from-source image
#   2  anything published: the release rows, the engine image
# Default 2, because an unset variable must never quietly under-optimise
# something that ships.
#
# The spread is wider than it looks. On a 24-core machine a cold build of the
# library plus the test suite takes 547 s at -O2 and 42 s at -O0; one module
# of generic JSON instances accounts for 397 s of the -O2 figure and holds
# everything that imports it behind that. The runtime it buys is real too:
# unoptimised generic instances encode and decode 10 to 20 % slower, which is
# why what ships stays at 2.
VOLCA_OPT_LEVEL="${VOLCA_OPT_LEVEL:-2}"

# Section splitting follows the same rule, for the same reason, and that is
# why it is decided here rather than left to cabal.project's `package *`.
# Splitting earns its keep on what ships: the executable prunes what nothing
# references and comes out several megabytes smaller. Nothing prunes the test
# suite, which links every section it was handed, and the default linker
# spends minutes resolving them (302 s and 2.3 GB against 3.6 s without, on a
# tree built at -O0, where there are more symbols to split). Dependencies keep
# splitting either way: they are prebuilt, and they are most of what the
# executable links.
if [ "$VOLCA_OPT_LEVEL" -lt 2 ]; then
    VOLCA_SPLIT_SECTIONS="  split-sections: False"
else
    VOLCA_SPLIT_SECTIONS="  split-sections: True"
fi

# Parallelism preamble shared by every LINK_MODE.
# Lives in cabal.project.local (not cabal.project) so that Docker builds –
# which copy only volca.cabal into the build context and write a
# minimal cabal.project without `packages: .` machinery – still get jobs +
# RTS allocation area + per-module GHC parallelism.
#
# shared / executable-dynamic sit here for the same reason: they apply to
# every mode, not one. On a dynamic GHC, which is what ghcup installs for
# macOS and for glibc Linux, cabal builds our library both ways unless told
# not to, and every module is compiled twice. The CI log shows it plainly: a
# `.dyn_o` next to the `.o` on every macOS Compiling line, where the Alpine
# leg shows the `.o` alone. It buys nothing anywhere here, since no module in
# this tree uses TemplateHaskell and the packaged binaries are standalone, so
# a dynamic copy of our own library is never loaded.
#
# Both fields are needed, not one. Up to cabal-install 3.12 they are combined
# with `liftM2 (||)`, so `shared: False` with the other unset is Nothing and
# the compiler's own default wins - True, on a dynamic GHC. 3.14 honours
# `shared` alone. Setting both makes the pair definite on either version.
#
# The price is `cabal repl` on a GHC that is itself dynamic: the interpreter
# loads packages the dynamic way only, so a repl on exe:volca or on the test
# suite, both of which depend on the library, can no longer link it. Nothing
# here runs a repl, and HLS is unaffected because it loads local components
# from source. If you want one, build that invocation with --enable-shared.
# That does not bring back a repl on the library itself: the solver archive is
# static, and the interpreter of a dynamic GHC loads no static library.
# In a checkout built before this line existed, delete dist-newstyle first:
# the stale .so is still there, and a repl would quietly load it.
cat > "$OUTPUT" << 'EOF'
jobs: $ncpus

shared: False
executable-dynamic: False

program-options
  ghc-options: -j +RTS -A128m -RTS

EOF

case "$LINK_MODE" in
    dynamic)
        # Shared linking, on glibc Linux. The solver archive needs
        # nothing named here: the C compiler that links brings the unwinder
        # and the C library, and GHC's own runtime already names the rest of
        # what the archive asks for.
        cat >> "$OUTPUT" << EOF
optimization: 2

extra-lib-dirs: $SOLVER_LIB_DIR

package volca
  optimization: $VOLCA_OPT_LEVEL
$VOLCA_SPLIT_SECTIONS
EOF
        ;;

    musl)
        # Fully static link for Linux against musl libc (Alpine). musl's
        # `dlopen` / `getaddrinfo` / NSS lookups all resolve inside a static
        # binary without dragging in the host's libc, so the resulting
        # executable is genuinely portable across Linux distros.
        #
        # -z stack-size=8388608: bake an 8 MB PT_GNU_STACK into the ELF.
        # musl reads this header at startup and uses it as the default
        # pthread stack size (its hardcoded fallback is 128 KB, vs glibc's
        # 8 MB picked up from RLIMIT_STACK). Foreign code called from Haskell
        # runs on the stack of the thread that calls it - the solver, zlib,
        # the C parts of the dependencies - and none of it was written with
        # 128 KB in mind. Setting it at link time covers every pthread the
        # binary creates.
        #
        # The solver archive asks for -lgcc_s and -lc. Neither is named: a
        # static link takes the unwinder from libgcc_eh and the C library by
        # itself, and Alpine has no static libgcc_s to find.
        cat >> "$OUTPUT" << EOF
optimization: 2
split-sections: True
executable-static: True

extra-lib-dirs: $SOLVER_LIB_DIR

package volca
  optimization: $VOLCA_OPT_LEVEL
$VOLCA_SPLIT_SECTIONS
  ghc-options: -optl-Wl,-z,stack-size=8388608
EOF
        ;;

    darwin)
        # macOS. The binary loads nothing but what a stock macOS has, and
        # runs on the version versions.env pins and later.
        #
        # -dead_strip_dylibs drops the load commands of dylibs nothing needs.
        #
        # The solver archive asks for -liconv, -lSystem, -lc and -lm. The
        # linker brings the system library and the C one itself; the other two
        # are named.
        DEPLOYMENT_TARGET="${MACOSX_DEPLOYMENT_TARGET:?MACOSX_DEPLOYMENT_TARGET must be set (source versions.env)}"
        cat >> "$OUTPUT" << EOF
optimization: 2
split-sections: True

extra-lib-dirs: $SOLVER_LIB_DIR

package volca
  optimization: $VOLCA_OPT_LEVEL
$VOLCA_SPLIT_SECTIONS
  ghc-options: -optl-lm -optl-liconv -optl-mmacosx-version-min=${DEPLOYMENT_TARGET} -optl-Wl,-dead_strip_dylibs
EOF
        ;;

    windows)
        # Windows/MSYS2. Auto-discover the MSYS2 and GCC library directories,
        # in the forward-slash drive-letter form (`C:/foo/bar`) that cabal and
        # clang want.
        if [[ -z "${MSYS2_LIB_DIR:-}" ]]; then
            MSYS2_LIB_DIR=$(cygpath -m /ucrt64/lib)
            : "${MSYS2_LIB_DIR:?cygpath -m /ucrt64/lib returned empty – is MSYS2 ucrt64 installed?}"
        fi
        if [[ -z "${GCC_LIB_DIR:-}" ]]; then
            GCC_LIB_DIR=$(find /ucrt64/lib/gcc/x86_64-w64-mingw32 -maxdepth 1 -type d 2>/dev/null | sort -V | tail -1)
            : "${GCC_LIB_DIR:?Could not locate GCC lib dir under /ucrt64/lib/gcc/x86_64-w64-mingw32 – install mingw-w64-ucrt-x86_64-gcc}"
            GCC_LIB_DIR=$(cygpath -m "$GCC_LIB_DIR")
        fi
        SOLVER_LIB_DIR=$(cygpath -m "$SOLVER_LIB_DIR")
        # The solver archive is built by MSYS2's Rust, so that it shares the C
        # library and the unwinder of everything else on this line. The system
        # libraries it asks for close the line, as the Rust compiler lists
        # them. It does not list the unwinder, which it needs all the same:
        # -lgcc_eh, placed before the -lgcc and -lpthread it leans on.
        cat >> "$OUTPUT" << EOF
optimization: 2
split-sections: True

extra-lib-dirs: $MSYS2_LIB_DIR
              , $SOLVER_LIB_DIR

package volca
  optimization: $VOLCA_OPT_LEVEL
$VOLCA_SPLIT_SECTIONS
  ghc-options: -optl-L$GCC_LIB_DIR -optl-L$MSYS2_LIB_DIR -optl-lgcc_eh -optl-lgcc -optl-lmingwex -optl-lpthread -optl-lmsvcrt -optl-lkernel32 -optl-lntdll -optl-luserenv -optl-lws2_32 -optl-ldbghelp

-- MSYS2's pkg-config finds zlib, so the zlib package would link the shared
-- zlib1.dll, which a user's machine does not have: volca.exe then fails to
-- start. Compile zlib's C sources in instead, as upstream advises
-- (haskell/zlib#65).
package zlib
  flags: +bundled-c-zlib
EOF
        ;;

    *)
        echo "ERROR: Unknown LINK_MODE: $LINK_MODE" >&2
        exit 1
        ;;
esac

# The same directory, for a project elsewhere that compiles the engine as one
# of its own packages and keeps its own configuration: it imports this file.
mkdir -p "$SOLVER_TARGET_DIR"
echo "extra-lib-dirs: $SOLVER_LIB_DIR" > "$SOLVER_TARGET_DIR/archive.project"

echo "Generated $OUTPUT (mode=$LINK_MODE)"
