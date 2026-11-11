#!/bin/bash
# Generate cabal.project.local for building volca.
# Shared between build.sh and Dockerfile.
#
# Optional env vars:
#   LINK_MODE               "dynamic" (default), "musl", "darwin", "windows"
#   OUTPUT_DIR              Where to write cabal.project.local (default: current dir)
#   VOLCA_OPT_LEVEL         0, 1 or 2 (default 2) - see the block below
#
# Output: writes cabal.project.local in OUTPUT_DIR

set -e

LINK_MODE="${LINK_MODE:-dynamic}"
OUTPUT="${OUTPUT_DIR:-.}/cabal.project.local"

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

# The sparse solver reads and writes its arrays unchecked, which is what makes
# it fast and what would let a wrong index reach a neighbour's memory without a
# word. Where the build only has to run the suite, every such access is
# checked instead, so the suite stops on the first one out of bounds.
if [ "$VOLCA_OPT_LEVEL" -eq 0 ]; then
    VOLCA_BOUNDS_CHECK="-fcheck-prim-bounds "
else
    VOLCA_BOUNDS_CHECK=""
fi

# Parallelism preamble shared by every LINK_MODE.
# Lives in cabal.project.local (not cabal.project) so that Docker builds –
# which copy only volca.cabal into the build context and write a minimal
# cabal.project without `packages: .` machinery – still get jobs + RTS
# allocation area + per-module GHC parallelism.
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
# In a checkout built before this line existed, delete dist-newstyle first:
# the stale .so is still there, and a repl would quietly load it.
cat > "$OUTPUT" << 'EOF'
jobs: $ncpus

shared: False
executable-dynamic: False

program-options
  ghc-options: -j +RTS -A128m -RTS

EOF

# What every mode says about volca itself. $1 is the mode's own link flags.
volca_package() {
    local options="$VOLCA_BOUNDS_CHECK$1"
    cat << EOF

package volca
  optimization: $VOLCA_OPT_LEVEL
$VOLCA_SPLIT_SECTIONS
EOF
    if [ -n "$options" ]; then
        echo "  ghc-options: $options"
    fi
}

case "$LINK_MODE" in
    dynamic)
        # Shared linking (glibc Linux, dev builds)
        {
            echo "optimization: 2"
            volca_package ""
        } >> "$OUTPUT"
        ;;

    musl)
        # Fully static link for Linux against musl libc (Alpine). musl's
        # `dlopen` / `getaddrinfo` / NSS lookups all resolve inside a static
        # binary without dragging in the host's libc, so the resulting
        # executable is genuinely portable across Linux distros.
        #
        # -z stack-size=8388608: musl gives a thread 128 KB of stack unless
        # the program header asks for more, where glibc gives 8 MB. The C
        # code the engine calls runs on those threads, and has only ever
        # shipped with 8 MB under it.
        {
            echo "optimization: 2"
            echo "split-sections: True"
            echo "executable-static: True"
            volca_package "-optl-Wl,-z,stack-size=8388608"
        } >> "$OUTPUT"
        ;;

    darwin)
        # -dead_strip_dylibs drops the load commands of dylibs nothing needs.
        DEPLOYMENT_TARGET="${MACOSX_DEPLOYMENT_TARGET:?MACOSX_DEPLOYMENT_TARGET must be set (source versions.env)}"
        {
            echo "optimization: 2"
            echo "split-sections: True"
            volca_package "-optl-mmacosx-version-min=${DEPLOYMENT_TARGET} -optl-Wl,-dead_strip_dylibs"
        } >> "$OUTPUT"
        ;;

    windows)
        {
            echo "optimization: 2"
            echo "split-sections: True"
            volca_package ""
            cat << 'EOF'

-- MSYS2's pkg-config finds zlib, so the zlib package would link the shared
-- zlib1.dll, which a user's machine does not have: volca.exe then fails to
-- start. Compile zlib's C sources in instead, as upstream advises
-- (haskell/zlib#65).
package zlib
  flags: +bundled-c-zlib
EOF
        } >> "$OUTPUT"
        ;;

    *)
        echo "ERROR: Unknown LINK_MODE: $LINK_MODE" >&2
        exit 1
        ;;
esac

echo "Generated $OUTPUT (mode=$LINK_MODE)"
