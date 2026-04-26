#!/bin/bash
# Generate cabal.project.local for building volca with MUMPS_SEQ.
# Shared between build.sh and Dockerfile.
#
# Optional env vars:
#   MUMPS_LIB_DIR           Path to MUMPS libraries (default: system)
#   MUMPS_INCLUDE_DIR       Path to MUMPS headers (default: /usr/include)
#   LINK_MODE               "dynamic" (default), "static", "darwin", "windows"
#   OUTPUT_DIR              Where to write cabal.project.local (default: current dir)
#
# Output: writes cabal.project.local in OUTPUT_DIR

set -e

MUMPS_LIB_DIR="${MUMPS_LIB_DIR:-/usr/lib/x86_64-linux-gnu}"
MUMPS_INCLUDE_DIR="${MUMPS_INCLUDE_DIR:-/usr/include}"
LINK_MODE="${LINK_MODE:-dynamic}"
OUTPUT="${OUTPUT_DIR:-.}/cabal.project.local"

case "$LINK_MODE" in
    dynamic)
        # Shared linking (Linux, macOS, Docker, dev builds)
        cat > "$OUTPUT" << EOF
optimization: 2

extra-lib-dirs: $MUMPS_LIB_DIR
extra-include-dirs: $MUMPS_INCLUDE_DIR
EOF
        ;;

    static)
        # Static linking: embed MUMPS/BLAS/LAPACK, keep gfortran/libc dynamic
        STATIC_LINK_FLAGS="-optl-L$MUMPS_LIB_DIR -optl-Wl,-Bstatic -optl-Wl,--start-group -optl-ldmumps_seq -optl-lmumps_common_seq -optl-lpord_seq -optl-lmpiseq_seq -optl-Wl,--end-group -optl-Wl,-Bdynamic -optl-llapack -optl-lblas -optl-lgfortran -optl-lquadmath -optl-lpthread -optl-lm -optl-ldl"
        cat > "$OUTPUT" << EOF
optimization: 2
split-sections: True

extra-lib-dirs: $MUMPS_LIB_DIR
extra-include-dirs: $MUMPS_INCLUDE_DIR

package mumps-hs
  extra-lib-dirs: $MUMPS_LIB_DIR
  ghc-options: $STATIC_LINK_FLAGS

package volca
  ghc-options: $STATIC_LINK_FLAGS
EOF
        ;;

    darwin)
        # macOS arm64: locally-built MUMPS (.a only) + Homebrew openblas + Homebrew gcc gfortran/quadmath.
        # ld64 picks .a from extra-lib-dirs when no .dylib is present, so no GNU -Bstatic/-Bdynamic.
        # Accelerate.framework is rejected: its LAPACK ABI does not match what build-mumps.sh emits.
        BREW_PREFIX="$(brew --prefix 2>/dev/null || echo /opt/homebrew)"
        OPENBLAS_PREFIX=$(brew --prefix openblas 2>/dev/null || echo "${BREW_PREFIX}/opt/openblas")
        # Homebrew gcc lays out libgfortran/libquadmath under lib/gcc/<major>/
        GFORTRAN_LIB_DIR=$(ls -d "${BREW_PREFIX}/Cellar/gcc/"*/lib/gcc/*/ 2>/dev/null | sort -V | tail -1)
        : "${GFORTRAN_LIB_DIR:?Could not locate Homebrew gcc libgfortran — install with: brew install gcc}"
        DEPLOYMENT_TARGET="${MACOSX_DEPLOYMENT_TARGET:-13.0}"
        DARWIN_LINK_FLAGS="-optl-L$MUMPS_LIB_DIR -optl-ldmumps_seq -optl-lmumps_common_seq -optl-lpord_seq -optl-lmpiseq_seq -optl-L${OPENBLAS_PREFIX}/lib -optl-lopenblas -optl-L${GFORTRAN_LIB_DIR} -optl-lgfortran -optl-lquadmath -optl-lpthread -optl-lm -optl-mmacosx-version-min=${DEPLOYMENT_TARGET}"
        cat > "$OUTPUT" << EOF
optimization: 2
split-sections: True

extra-lib-dirs: $MUMPS_LIB_DIR
extra-include-dirs: $MUMPS_INCLUDE_DIR

package mumps-hs
  extra-lib-dirs: $MUMPS_LIB_DIR
  ghc-options: $DARWIN_LINK_FLAGS

package volca
  ghc-options: $DARWIN_LINK_FLAGS
EOF
        ;;

    windows)
        # Windows/MSYS2: MinGW + OpenBLAS
        : "${MSYS2_LIB_DIR:?MSYS2_LIB_DIR is required for windows mode}"
        : "${GCC_LIB_DIR:?GCC_LIB_DIR is required for windows mode}"
        cat > "$OUTPUT" << EOF
optimization: 2
split-sections: True

extra-lib-dirs: $MUMPS_LIB_DIR
              , $MSYS2_LIB_DIR
extra-include-dirs: $MUMPS_INCLUDE_DIR

package volca
  ghc-options: -optl-Wl,--allow-multiple-definition -optl-L$GCC_LIB_DIR -optl-L$MSYS2_LIB_DIR -optl-L$MUMPS_LIB_DIR -optl-ldmumps_seq -optl-lmumps_common_seq -optl-lpord_seq -optl-lmpiseq_seq -optl-lopenblas -optl-lgfortran -optl-lgcc -optl-lquadmath -optl-lmingwex -optl-lpthread -optl-lmsvcrt
EOF
        ;;

    *)
        echo "ERROR: Unknown LINK_MODE: $LINK_MODE" >&2
        exit 1
        ;;
esac

echo "Generated $OUTPUT (mode=$LINK_MODE)"
