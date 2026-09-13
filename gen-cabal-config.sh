#!/bin/bash
# Generate cabal.project.local for building volca with MUMPS_SEQ.
# Shared between build.sh and Dockerfile.
#
# Optional env vars:
#   MUMPS_LIB_DIR           Path to MUMPS libraries (default: system)
#   MUMPS_INCLUDE_DIR       Path to MUMPS headers (default: /usr/include)
#   LINK_MODE               "dynamic" (default), "musl", "darwin", "windows"
#   OUTPUT_DIR              Where to write cabal.project.local (default: current dir)
#   VOLCA_OPT_LEVEL         0, 1 or 2 (default 2) - see the block below
#
# Output: writes cabal.project.local in OUTPUT_DIR

set -e

MUMPS_LIB_DIR="${MUMPS_LIB_DIR:-/usr/lib/x86_64-linux-gnu}"
MUMPS_INCLUDE_DIR="${MUMPS_INCLUDE_DIR:-/usr/include}"
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

# Parallelism preamble shared by every LINK_MODE.
# Lives in cabal.project.local (not cabal.project) so that Docker builds -
# which copy only volca.cabal + mumps-hs/ into the build context and write a
# minimal cabal.project without `packages: .` machinery - still get jobs +
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
        # Shared linking (Linux, macOS, Docker, dev builds)
        cat >> "$OUTPUT" << EOF
optimization: 2

extra-lib-dirs: $MUMPS_LIB_DIR
extra-include-dirs: $MUMPS_INCLUDE_DIR

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
        # Alpine packages ship LAPACK/BLAS as shared libs only (no .a), so
        # OpenBLAS - which bundles both BLAS and LAPACK in a single static
        # archive - must be built from source and pointed at via OPENBLAS_LIB_DIR.
        : "${OPENBLAS_LIB_DIR:?OPENBLAS_LIB_DIR is required for musl mode (path to libopenblas.a)}"
        case "$(uname -m)" in
            x86_64|amd64) QUADMATH_FLAG="-optl-lquadmath" ;;
            *)            QUADMATH_FLAG="" ;;
        esac
        # --gc-sections drops unreferenced sections from the final exe.
        # Effective on the C/Fortran archives that were compiled with
        # -ffunction-sections / -fdata-sections (OpenBLAS in our pipeline);
        # harmless on the others.
        #
        # -z stack-size=8388608: bake an 8 MB PT_GNU_STACK into the ELF.
        # musl reads this header at startup and uses it as the default
        # pthread stack size (its hardcoded fallback is 128 KB, vs glibc's
        # 8 MB picked up from RLIMIT_STACK). OpenBLAS DYNAMIC_ARCH Fortran
        # kernels have large auto-arrays that overflow 128 KB on the first
        # BLAS3 call inside MUMPS factorization (SIGSEGV / exit 139).
        # Setting it at link time covers every pthread the binary creates
        # - RTS capabilities and OpenBLAS workers alike - without patching
        # OpenBLAS source. (An earlier attempt to sed the stack size into
        # OpenBLAS's blas_server.c was a no-op: the relevant block sits
        # under #ifdef NEED_STACKATTR, which blas_server.c #undef's
        # unconditionally on Linux.)
        MUSL_LINK_FLAGS="-optl-L$MUMPS_LIB_DIR -optl-L$OPENBLAS_LIB_DIR -optl-Wl,--gc-sections -optl-Wl,-z,stack-size=8388608 -optl-Wl,--start-group -optl-ldmumps_seq -optl-lmumps_common_seq -optl-lpord_seq -optl-lmpiseq_seq -optl-lopenblas -optl-lgfortran $QUADMATH_FLAG -optl-Wl,--end-group -optl-lpthread -optl-lm"
        cat >> "$OUTPUT" << EOF
optimization: 2
split-sections: True
executable-static: True

extra-lib-dirs: $MUMPS_LIB_DIR
                $OPENBLAS_LIB_DIR
extra-include-dirs: $MUMPS_INCLUDE_DIR

package volca
  optimization: $VOLCA_OPT_LEVEL
$VOLCA_SPLIT_SECTIONS
  ghc-options: $MUSL_LINK_FLAGS
EOF
        ;;

    darwin)
        # macOS: locally-built MUMPS (.a only) + Homebrew gcc gfortran/quadmath.
        # ld64 picks .a from extra-lib-dirs when no .dylib is present, so no GNU -Bstatic/-Bdynamic.
        # Accelerate.framework is rejected: its LAPACK ABI does not match what build-mumps.sh emits.
        #
        # arm64 links BLAS and the Fortran runtime from their .a by absolute path,
        # never via -l: Homebrew ships both .a and .dylib, ld64 has no -Bstatic to
        # express the preference, and it picks the .dylib. That produced a binary whose
        # LC_LOAD_DYLIB entries point into the build machine's Homebrew prefix, so the
        # shipped tarball aborted at dyld ("Library not loaded: .../libopenblas.0.dylib")
        # on any Mac without those formulas. Linux already links these statically (musl
        # mode); this gives macOS arm64 the same standalone binary.
        #
        # OpenBLAS comes from the same source build musl mode uses, not from Homebrew:
        # the bottled libopenblas.a is the OpenMP variant, whose __kmpc_* / omp_*
        # references only the dylib resolved on its own. Building it with USE_OPENMP=0
        # settles that instead of adding libomp - one more Homebrew dependency to keep
        # out of the shipped binary.
        #
        # x86_64 keeps the dynamic link, and the Homebrew dependency with it, because
        # no OpenBLAS build works there: DYNAMIC_ARCH pulls in older kernels that
        # hard-code `.align 32768`, which Mach-O caps at 4 KB and ld64 silently reduces
        # (SIGBUS on the first call), while naming one TARGET yields a library whose own
        # unit tests return denormals for min(). Both are upstream ground.
        BREW_PREFIX="$(brew --prefix 2>/dev/null || echo /opt/homebrew)"
        # Homebrew gcc lays out libgfortran/libquadmath under lib/gcc/<major>/
        GFORTRAN_LIB_DIR=$(ls -d "${BREW_PREFIX}/Cellar/gcc/"*/lib/gcc/*/ 2>/dev/null | sort -V | tail -1)
        : "${GFORTRAN_LIB_DIR:?Could not locate Homebrew gcc libgfortran - install with: brew install gcc}"
        GFORTRAN_LIB_DIR="${GFORTRAN_LIB_DIR%/}"
        DEPLOYMENT_TARGET="${MACOSX_DEPLOYMENT_TARGET:?MACOSX_DEPLOYMENT_TARGET must be set (source versions.env)}"
        # MUMPS arrives as .a either way; only how BLAS and the Fortran runtime
        # arrive differs between the two architectures.
        DARWIN_MUMPS_FLAGS="-optl-L$MUMPS_LIB_DIR -optl-ldmumps_seq -optl-lmumps_common_seq -optl-lpord_seq -optl-lmpiseq_seq"
        # -dead_strip_dylibs only drops load commands for dylibs nothing needs,
        # which is safe. Plain -dead_strip is not, once OpenBLAS is linked
        # statically: ld64 splits sections into atoms at symbol boundaries, and
        # a local assembler label like .L2_0 is not a symbol, so hand-written
        # kernel code reached by a jump from a neighbouring atom can be dropped,
        # leaving a hole that faults when execution lands in it. Nothing here
        # proves that has happened; the few kilobytes are not worth the risk.
        DARWIN_TAIL_FLAGS="-optl-lpthread -optl-lm -optl-mmacosx-version-min=${DEPLOYMENT_TARGET} -optl-Wl,-dead_strip_dylibs"

        if [[ "$(uname -m)" == "arm64" ]]; then
            : "${OPENBLAS_LIB_DIR:?OPENBLAS_LIB_DIR is required for darwin arm64 (path to a libopenblas.a built with USE_OPENMP=0 - see .github/actions/setup-haskell-env)}"
            # Ordered dependent-before-dependency: openblas calls into libgfortran, which
            # calls into libquadmath. libgcc.a comes last and is located by asking the
            # compiler driver rather than guessing its Cellar layout - gcc keeps it under
            # lib/gcc/<major>/gcc/<triple>/<major>/, not next to libgfortran.a. It resolves
            # the emutls/soft-arithmetic symbols libgfortran.a leaves undefined.
            DARWIN_STATIC_LIBS=(
                "${OPENBLAS_LIB_DIR}/libopenblas.a"
                "${GFORTRAN_LIB_DIR}/libgfortran.a"
                "${GFORTRAN_LIB_DIR}/libquadmath.a"
            )
            # An unanswered driver would drop libgcc.a from the link and surface as an
            # undefined ___emutls_get_address far from its cause, so an empty answer is
            # an error like a missing archive - the loop below reports it either way.
            GCC_A=$("${BREW_PREFIX}/bin/gfortran" -print-libgcc-file-name 2>/dev/null || true)
            DARWIN_STATIC_LIBS+=("${GCC_A:-<gfortran -print-libgcc-file-name answered nothing>}")
            DARWIN_NUMERIC_FLAGS=""
            for lib in "${DARWIN_STATIC_LIBS[@]}"; do
                if [[ ! -f "$lib" ]]; then
                    echo "ERROR: static library not found: $lib" >&2
                    echo "       The shipped binary must not depend on Homebrew dylibs." >&2
                    echo "       Fortran runtime: brew install gcc. OpenBLAS: build it with" >&2
                    echo "       NO_SHARED=1 USE_OPENMP=0 (see .github/actions/setup-haskell-env)." >&2
                    exit 1
                fi
                # OpenBLAS goes in whole, the rest on demand. ld64 pulls members out
                # of an archive in one pass, driven by symbols undefined so far, and
                # OpenBLAS reaches its kernels through tables of function pointers -
                # a reference no symbol resolution can see, so the member holding a
                # kernel can go unpulled and leave its pointer zero.
                case "$lib" in
                    *libopenblas.a) DARWIN_NUMERIC_FLAGS="$DARWIN_NUMERIC_FLAGS -optl-Wl,-force_load,$lib" ;;
                    *)              DARWIN_NUMERIC_FLAGS="$DARWIN_NUMERIC_FLAGS -optl$lib" ;;
                esac
            done
        else
            OPENBLAS_PREFIX=$(brew --prefix openblas 2>/dev/null || echo "${BREW_PREFIX}/opt/openblas")
            DARWIN_NUMERIC_FLAGS=" -optl-L${OPENBLAS_PREFIX}/lib -optl-lopenblas -optl-L${GFORTRAN_LIB_DIR} -optl-lgfortran -optl-lquadmath"
        fi
        DARWIN_LINK_FLAGS="$DARWIN_MUMPS_FLAGS$DARWIN_NUMERIC_FLAGS $DARWIN_TAIL_FLAGS"
        cat >> "$OUTPUT" << EOF
optimization: 2
split-sections: True

extra-lib-dirs: $MUMPS_LIB_DIR
extra-include-dirs: $MUMPS_INCLUDE_DIR

package mumps-hs
  extra-lib-dirs: $MUMPS_LIB_DIR
  ghc-options: $DARWIN_LINK_FLAGS

package volca
  optimization: $VOLCA_OPT_LEVEL
$VOLCA_SPLIT_SECTIONS
  ghc-options: $DARWIN_LINK_FLAGS
EOF
        ;;

    windows)
        # Windows/MSYS2: MinGW + OpenBLAS
        # Auto-discover MSYS2/GCC paths and convert POSIX-style MUMPS paths
        # to Windows form. Callers running under MSYS2 bash (build.sh,
        # prebuild-cabal-store.yml) used to duplicate this block; factoring
        # it here keeps the per-caller code to LINK_MODE=windows.
        if [[ -z "${MSYS2_LIB_DIR:-}" ]]; then
            MSYS2_LIB_DIR=$(cygpath -m /ucrt64/lib)
            : "${MSYS2_LIB_DIR:?cygpath -m /ucrt64/lib returned empty - is MSYS2 ucrt64 installed?}"
        fi
        if [[ -z "${GCC_LIB_DIR:-}" ]]; then
            GCC_LIB_DIR=$(find /ucrt64/lib/gcc/x86_64-w64-mingw32 -maxdepth 1 -type d 2>/dev/null | sort -V | tail -1)
            : "${GCC_LIB_DIR:?Could not locate GCC lib dir under /ucrt64/lib/gcc/x86_64-w64-mingw32 - install mingw-w64-ucrt-x86_64-gcc}"
            GCC_LIB_DIR=$(cygpath -m "$GCC_LIB_DIR")
        fi
        # Cabal + clang on Windows want forward-slash drive-letter paths
        # (`C:/foo/bar`), not the MSYS2 `/c/foo/bar` form. Convert if needed.
        win_path() { echo "$1" | sed 's|^/\([a-zA-Z]\)/|\1:/|'; }
        case "$MUMPS_LIB_DIR" in
            /[a-zA-Z]/*) MUMPS_LIB_DIR=$(win_path "$MUMPS_LIB_DIR") ;;
        esac
        case "$MUMPS_INCLUDE_DIR" in
            /[a-zA-Z]/*) MUMPS_INCLUDE_DIR=$(win_path "$MUMPS_INCLUDE_DIR") ;;
        esac
        cat >> "$OUTPUT" << EOF
optimization: 2
split-sections: True

extra-lib-dirs: $MUMPS_LIB_DIR
              , $MSYS2_LIB_DIR
extra-include-dirs: $MUMPS_INCLUDE_DIR

package volca
  optimization: $VOLCA_OPT_LEVEL
$VOLCA_SPLIT_SECTIONS
  ghc-options: -optl-Wl,--allow-multiple-definition -optl-L$GCC_LIB_DIR -optl-L$MSYS2_LIB_DIR -optl-L$MUMPS_LIB_DIR -optl-ldmumps_seq -optl-lmumps_common_seq -optl-lpord_seq -optl-lmpiseq_seq -optl-lopenblas -optl-lgfortran -optl-lgcc -optl-lquadmath -optl-lmingwex -optl-lpthread -optl-lmsvcrt
EOF
        ;;

    *)
        echo "ERROR: Unknown LINK_MODE: $LINK_MODE" >&2
        exit 1
        ;;
esac

echo "Generated $OUTPUT (mode=$LINK_MODE)"
