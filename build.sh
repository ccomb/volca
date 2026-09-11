#!/bin/bash

# =============================================================================
# volca build script
# =============================================================================
# This script builds volca with MUMPS_SEQ direct solver.
# Works on Linux, macOS, and Windows (via MSYS2).
#
# Usage:
#   ./build.sh [options]
#
# Options:
#   --help              Show this help message
#   --clean             Clean build artifacts before building
#   --all               Force clean rebuild
#   --test              Run tests after building. Compiles volca's own modules
#                       at -O0, since the binary only has to make the suite
#                       pass; VOLCA_OPT_LEVEL overrides that, and a build
#                       without --test stays at -O1 because it is going to run
#   --coverage          Run tests with coverage and generate HTML report (implies --test)
#   --werror            Treat warnings as errors (-Werror) when compiling — the
#                       strict gate CI uses; -Wall-clean is a hard project rule
#   --static            Build a statically-linked binary
#   --no-optimize       Skip strip + UPX (preserves dylib load commands so
#                       downstream tooling like dylibbundler / install_name_tool
#                       can rewrite them — required for the macOS .app)
#   --optimize-only     Skip the entire build: just strip + UPX + (re-)sign the
#                       binary produced by a previous run. Used in CI to ship a
#                       small artifact AFTER tests have run on the unstripped
#                       binary (UPX'd Mach-O on macOS arm64 fails to launch
#                       under cabal test even after ad-hoc re-sign).
#
# Examples:
#   ./build.sh                      # Build
#   ./build.sh --test               # Build and run tests
#   ./build.sh --static             # Build statically-linked binary
#
# Windows:
#   Run from MSYS2 UCRT64 terminal:
#   $ ./build.sh
#
# =============================================================================

set -e

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Source shared library functions
if [[ -f "$SCRIPT_DIR/lib.sh" ]]; then
    # shellcheck source=lib.sh
    source "$SCRIPT_DIR/lib.sh"
else
    echo "ERROR: lib.sh not found in $SCRIPT_DIR"
    exit 1
fi

# Source version definitions (set -a exports all for subprocesses)
set -a
if [[ -f "$SCRIPT_DIR/versions.env" ]]; then
    # shellcheck source=versions.env
    source "$SCRIPT_DIR/versions.env"
else
    log_error "versions.env not found in $SCRIPT_DIR"
    exit 1
fi
set +a

# Detect OS
OS=$(detect_os)

# On macOS, pin the deployment target floor for every link step so binaries
# produced on a recent Mac still run on the version pinned in versions.env
# (currently Ventura 13.0). GHC, rustc, clang, and Homebrew gcc all honor
# this env var natively. `set -a` above auto-exported the versions.env value;
# the explicit export here is just for readability.
if [[ "$OS" == "macos" ]]; then
    : "${MACOSX_DEPLOYMENT_TARGET:?MACOSX_DEPLOYMENT_TARGET must be set in versions.env}"
    export MACOSX_DEPLOYMENT_TARGET
fi

# Strip + UPX + (re-)sign $1 in place. Idempotent: a binary already UPX'd
# is left as-is. Linker output is the input; only platform-correct tooling
# is invoked. Used both at the end of a normal build and as the sole action
# of `--optimize-only`.
optimize_volca_binary() {
    local bin_path="$1"
    if [[ -z "$bin_path" || ! -f "$bin_path" ]]; then
        log_error "optimize_volca_binary: binary not found at '$bin_path'"
        return 1
    fi
    if upx -t "$bin_path" &>/dev/null; then
        local sz; sz=$(du -h "$bin_path" | cut -f1)
        log_info "Binary already optimized ($sz), skipping strip/UPX"
        return 0
    fi

    local original_size stripped_size final_size
    original_size=$(du -h "$bin_path" | cut -f1)
    log_info "Binary size before optimization: $original_size"

    log_info "Stripping debug symbols..."
    if [[ "$OS" == "macos" ]]; then
        strip -x "$bin_path"
    else
        strip --strip-all "$bin_path"
    fi
    stripped_size=$(du -h "$bin_path" | cut -f1)
    log_info "After strip: $stripped_size"

    # Skip UPX on Windows (Defender flags UPX'd .exe as malicious, breaking the
    # NSIS installer) and on macOS (UPX 5.x with --force-macos compresses arm64
    # Mach-O fine — observed 104 M -> 48 M — but the resulting binary is killed
    # by the kernel at fork with SIGKILL ("Killed: 9"), even after `codesign
    # --force --sign -` re-signs it. This is an upstream UPX/Mach-O issue, not
    # something we can paper over here. macOS arm64 still gets ad-hoc signed
    # because strip can also invalidate the linker's signature.
    if [[ "$OS" == "windows" ]]; then
        log_success "Binary optimized: $original_size -> $stripped_size (stripped, no UPX on Windows)"
        return 0
    fi
    if [[ "$OS" == "macos" ]]; then
        codesign --force --sign - "$bin_path"
        log_success "Binary optimized: $original_size -> $stripped_size (stripped + ad-hoc signed, no UPX on macOS arm64)"
        return 0
    fi

    log_info "Compressing with UPX (default level)..."
    if upx "$bin_path"; then
        final_size=$(du -h "$bin_path" | cut -f1)
        log_success "Binary optimized: $original_size -> $stripped_size (stripped) -> $final_size (compressed)"
    else
        log_warn "UPX compression failed — using stripped binary ($stripped_size)"
    fi
}

# Defaults
FORCE_REBUILD=false
RUN_TESTS=false
CLEAN_BUILD=false
COVERAGE=false
STATIC_BUILD=false
SKIP_OPTIMIZE=false
OPTIMIZE_ONLY=false
WERROR=false

# -----------------------------------------------------------------------------
# Parse arguments
# -----------------------------------------------------------------------------

while [[ $# -gt 0 ]]; do
    case $1 in
        --help|-h)
            show_help "$0"
            ;;
        --clean)
            CLEAN_BUILD=true
            shift
            ;;
        --all)
            FORCE_REBUILD=true
            shift
            ;;
        --test)
            RUN_TESTS=true
            shift
            ;;
        --coverage)
            COVERAGE=true
            RUN_TESTS=true
            shift
            ;;
        --werror)
            WERROR=true
            shift
            ;;
        --static)
            STATIC_BUILD=true
            shift
            ;;
        --no-optimize)
            SKIP_OPTIMIZE=true
            shift
            ;;
        --optimize-only)
            OPTIMIZE_ONLY=true
            shift
            ;;
        *)
            log_error "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

# -Wall-clean is a hard project rule; --werror turns it into a build failure.
# Kept out of the .cabal file on purpose: a stray warning shouldn't block an
# everyday local `cabal build`, only the CI gate (and anyone opting in here).
STRICT_OPTS=()
if [[ "$WERROR" == "true" ]]; then
    STRICT_OPTS=(--ghc-options=-Werror)
fi

echo ""
log_info "Build configuration:"
log_info "  OS: $OS"
if [[ "$STATIC_BUILD" == "true" ]]; then
    log_info "  Static linking: enabled"
fi
echo ""

# --optimize-only: skip the entire build/test pipeline and just optimize the
# binary produced by an earlier run. Needs `cabal` (to locate the binary),
# `strip`, and `upx` — but none of the heavy build deps (gcc, MUMPS, ...).
if [[ "$OPTIMIZE_ONLY" == "true" ]]; then
    if ! command -v cabal &>/dev/null; then
        log_error "--optimize-only requires cabal in PATH to locate the binary"
        exit 1
    fi
    VOLCA_BIN_PATH=$(cabal list-bin exe:volca 2>/dev/null || true)
    if [[ -z "$VOLCA_BIN_PATH" || ! -f "$VOLCA_BIN_PATH" ]]; then
        log_error "--optimize-only: no built volca binary found (run ./build.sh first)"
        exit 1
    fi
    optimize_volca_binary "$VOLCA_BIN_PATH"
    exit 0
fi

# Check MSYS2 environment on Windows
if [[ "$OS" == "windows" ]]; then
    if ! require_msys2_ucrt64; then
        exit 1
    fi
fi

# -----------------------------------------------------------------------------
# Check dependencies
# -----------------------------------------------------------------------------

log_info "Checking dependencies..."

MISSING_DEPS=false

# Required build tools (platform-specific)
if [[ "$OS" == "windows" ]]; then
    for cmd in gcc make cmake python3 curl tar git; do
        if ! check_command "$cmd"; then
            MISSING_DEPS=true
        fi
    done
else
    for cmd in gcc g++ make python3 curl tar; do
        if ! check_command "$cmd"; then
            MISSING_DEPS=true
        fi
    done
fi

# Haskell toolchain
for cmd in ghc cabal; do
    if ! check_command "$cmd"; then
        MISSING_DEPS=true
        log_warn "Install GHC and Cabal via ghcup: https://www.haskell.org/ghcup/"
    fi
done

# UPX (needed for binary compression)
if ! check_command "upx"; then
    MISSING_DEPS=true
    if [[ "$OS" == "windows" ]]; then
        log_warn "Install UPX with: pacman -S mingw-w64-ucrt-x86_64-upx"
    elif [[ "$OS" == "macos" ]]; then
        log_warn "Install UPX with: brew install upx"
    else
        log_warn "Install UPX with: sudo apt install upx-ucl  (or: sudo pacman -S upx)"
    fi
fi

# Required C libraries (needed by Haskell zlib package)
if [[ "$OS" != "windows" ]]; then
    if pkg-config --exists zlib 2>/dev/null || cc -lz -shared -o /dev/null 2>/dev/null; then
        log_success "zlib development headers found"
    else
        MISSING_DEPS=true
        if [[ "$OS" == "macos" ]]; then
            log_error "zlib not found - install with: brew install zlib"
        else
            log_error "zlib not found - install with: sudo apt install zlib1g-dev"
        fi
    fi
fi

# Check for MUMPS sequential solver
MUMPS_FOUND=false
MUMPS_BUILT_LOCALLY=false
LOCAL_MUMPS_DIR="$SCRIPT_DIR/deps/mumps"

# Always prefer a locally-built MUMPS (deps/mumps/) — consistent version on all platforms
if [[ -f "$LOCAL_MUMPS_DIR/lib/libdmumps_seq.a" ]]; then
    MUMPS_FOUND=true
    MUMPS_BUILT_LOCALLY=true
    MUMPS_LIB_DIR="$LOCAL_MUMPS_DIR/lib"
    MUMPS_INCLUDE_DIR="$LOCAL_MUMPS_DIR/include"
    log_success "MUMPS found (local build): $MUMPS_LIB_DIR"
elif [[ "$OS" == "windows" ]]; then
    # MSYS2 packages
    if [[ -f "/ucrt64/lib/libdmumps_seq.a" ]] || [[ -f "/ucrt64/lib/libdmumps.a" ]]; then
        MUMPS_FOUND=true
        MUMPS_LIB_DIR="/ucrt64/lib"
        MUMPS_INCLUDE_DIR="/ucrt64/include"
        log_success "MUMPS found: $MUMPS_LIB_DIR"
    fi
elif [[ "$OS" == "macos" ]]; then
    # Homebrew
    if brew --prefix mumps &>/dev/null; then
        MUMPS_PREFIX=$(brew --prefix mumps)
        MUMPS_FOUND=true
        MUMPS_LIB_DIR="$MUMPS_PREFIX/lib"
        MUMPS_INCLUDE_DIR="$MUMPS_PREFIX/include"
        log_success "MUMPS found: $MUMPS_LIB_DIR"
    fi
else
    # Linux: check system install. Use the multiarch path so the same
    # check works on amd64 (/usr/lib/x86_64-linux-gnu) and arm64
    # (/usr/lib/aarch64-linux-gnu); fall back to /usr/lib for distros
    # that don't use multiarch.
    SYS_LIBDIR=/usr/lib/$(gcc -print-multiarch 2>/dev/null)
    [[ -d "$SYS_LIBDIR" ]] || SYS_LIBDIR=/usr/lib
    if [[ -f "$SYS_LIBDIR/libdmumps_seq.so" ]] || [[ -f "$SYS_LIBDIR/libdmumps_seq.a" ]]; then
        MUMPS_FOUND=true
        MUMPS_LIB_DIR="$SYS_LIBDIR"
        MUMPS_INCLUDE_DIR="/usr/include"
        log_success "MUMPS_SEQ found (system): $MUMPS_LIB_DIR"
    fi
fi

# Source build fallback when no system/brew/MSYS2 package is found
if [[ "$MUMPS_FOUND" != "true" ]]; then
    log_info "MUMPS not found; building ${MUMPS_VERSION} from source (this may take a few minutes)..."
    if [[ "$OS" == "windows" ]]; then
        # Ensure Fortran compiler is present (build tool, not the library we're compiling)
        if ! command -v gfortran &>/dev/null; then
            log_info "gfortran not found, installing via pacman..."
            pacman -S --noconfirm mingw-w64-ucrt-x86_64-gcc-fortran
        fi
        # Detect available BLAS; install OpenBLAS if nothing is found
        if [[ -f "/ucrt64/lib/libopenblas.a" ]]; then
            BLAS_FLAGS="-lopenblas"
        elif [[ -f "/ucrt64/lib/liblapack.a" ]] && [[ -f "/ucrt64/lib/libblas.a" ]]; then
            BLAS_FLAGS="-llapack -lblas"
        else
            log_info "No BLAS library found, installing OpenBLAS via pacman..."
            pacman -S --noconfirm mingw-w64-ucrt-x86_64-openblas
            BLAS_FLAGS="-lopenblas"
        fi
    elif [[ "$OS" == "macos" ]]; then
        # gfortran ships with the Homebrew gcc package
        if ! command -v gfortran &>/dev/null; then
            log_info "gfortran not found, installing gcc via brew..."
            brew install gcc
        fi
        # Use Homebrew openblas — works uniformly on Intel and Apple Silicon
        if ! brew --prefix openblas &>/dev/null; then
            log_info "openblas not found, installing via brew..."
            brew install openblas
        fi
        OPENBLAS_PREFIX=$(brew --prefix openblas)
        BLAS_FLAGS="-L${OPENBLAS_PREFIX}/lib -lopenblas"
        export CPPFLAGS="-I${OPENBLAS_PREFIX}/include ${CPPFLAGS:-}"
    else
        BLAS_FLAGS="-llapack -lblas"
    fi
    MUMPS_VERSION="$MUMPS_VERSION" \
    OUTPUT_DIR="$LOCAL_MUMPS_DIR" \
    BUILD_DIR="$SCRIPT_DIR/deps/build" \
    BLAS_LIBS="$BLAS_FLAGS" \
    "$SCRIPT_DIR/build-mumps.sh"
    MUMPS_FOUND=true
    MUMPS_BUILT_LOCALLY=true
    MUMPS_LIB_DIR="$LOCAL_MUMPS_DIR/lib"
    MUMPS_INCLUDE_DIR="$LOCAL_MUMPS_DIR/include"
fi

if [[ "$MUMPS_FOUND" != "true" ]]; then
    MISSING_DEPS=true
    log_error "MUMPS sequential solver not found"
    if [[ "$OS" == "windows" ]]; then
        log_error "Install with: pacman -S mingw-w64-ucrt-x86_64-mumps"
    elif [[ "$OS" == "macos" ]]; then
        log_error "Install with: brew install mumps"
    else
        log_error "Install with: sudo apt install libmumps-seq-dev"
    fi
fi

if [[ "$MISSING_DEPS" == "true" ]]; then
    log_error "Missing required dependencies. Please install them and try again."
    echo ""
    if [[ "$OS" == "windows" ]]; then
        echo "On Windows (MSYS2 UCRT64):"
        echo "  pacman -S mingw-w64-ucrt-x86_64-gcc mingw-w64-ucrt-x86_64-mumps make python git"
        echo ""
    elif [[ "$OS" == "macos" ]]; then
        echo "On macOS:"
        echo "  brew install gcc python3 curl node mumps"
        echo ""
    else
        echo "On Debian/Ubuntu:"
        echo "  sudo apt install build-essential python3 curl zlib1g-dev libmumps-seq-dev"
        echo ""
        echo "On Fedora:"
        echo "  sudo dnf install gcc gcc-c++ make python3 curl zlib-devel MUMPS-devel"
        echo ""
        echo "On Arch Linux:"
        echo "  sudo pacman -S base-devel python curl zlib mumps"
        echo ""
    fi
    echo "For Haskell toolchain:"
    echo "  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"
    exit 1
fi

# -----------------------------------------------------------------------------
# Check tool versions
# -----------------------------------------------------------------------------

log_info "Checking tool versions..."

# Check GHC version
GHC_ACTUAL=$(ghc --numeric-version)
check_version "GHC" "$GHC_ACTUAL" "$GHC_VERSION"

echo ""

# -----------------------------------------------------------------------------
# Clean if requested
# -----------------------------------------------------------------------------

if [[ "$CLEAN_BUILD" == "true" ]] || [[ "$FORCE_REBUILD" == "true" ]]; then
    log_info "Cleaning build artifacts..."
    cd "$SCRIPT_DIR"
    rm -rf dist-newstyle cabal.project.local
    log_success "Clean complete"
    echo ""
fi

# -----------------------------------------------------------------------------
# Build volca
# -----------------------------------------------------------------------------

log_info "Building volca..."
cd "$SCRIPT_DIR"

# Generate Version.hs with build info
BUILD_TARGET="$OS" ./gen-version.sh

# Write cabal.project.local with library paths
if [[ "$OS" == "windows" ]]; then
    LINK_MODE="windows"
    # MSYS2/GCC discovery + POSIX→Windows MUMPS path conversion live in
    # gen-cabal-config.sh's windows branch.
elif [[ "$OS" == "macos" ]] && [[ "$MUMPS_BUILT_LOCALLY" == "true" ]]; then
    # Darwin's ld64 doesn't support GNU -Wl,-Bstatic / -Bdynamic, so the static
    # mode below can't be used as-is. The darwin branch of gen-cabal-config.sh
    # says the rest, including why arm64 and x86_64 differ there.
    LINK_MODE="darwin"
elif [[ -f /etc/alpine-release ]] && { [[ "$STATIC_BUILD" == "true" ]] || [[ "$MUMPS_BUILT_LOCALLY" == "true" ]]; }; then
    # Alpine host (musl libc): fully-static binary linked against musl +
    # OpenBLAS-from-source. The only fully-portable Linux static path we
    # still support.
    LINK_MODE="musl"
    : "${OPENBLAS_LIB_DIR:?OPENBLAS_LIB_DIR required for musl builds (path to libopenblas.a — build via prebuild-openblas or apk add openblas-static + headers)}"
elif [[ "$STATIC_BUILD" == "true" ]]; then
    log_error "--static is only supported on Alpine/musl. Use the Docker build or run inside an Alpine container."
    exit 1
else
    LINK_MODE="dynamic"
fi

# A build run from a working copy is not shipped, so it defaults below the -O2
# gen-cabal-config.sh keeps for artifacts. Which level below follows what the
# build is for, the same rule that file states:
#
#   --test   0   the binary only has to make the suite pass, and one module of
#                generic JSON instances holds the whole compile behind it above
#                that level
#   plain    1   the binary is going to be run, and unoptimised generic
#                instances encode and decode 10 to 20 % slower
#
# The two do not share a build tree - cabal puts level 0 under `noopt/` - so
# alternating between them costs one cold build each and then stays
# incremental on both sides.
#
# An explicit VOLCA_OPT_LEVEL wins over both, which is how the release rows of
# CI ask for 2 while its test rows ask for 0.
if [[ "$RUN_TESTS" == "true" ]]; then
    DEFAULT_OPT_LEVEL=0
else
    DEFAULT_OPT_LEVEL=1
fi
OPT_LEVEL="${VOLCA_OPT_LEVEL:-$DEFAULT_OPT_LEVEL}"

# Said out loud because the level decides how fast the binary runs, and a
# developer who builds with --test and then serves that binary would otherwise
# find it slow with nothing on screen to say why.
log_info "Optimization level: -O$OPT_LEVEL"

MUMPS_LIB_DIR="$MUMPS_LIB_DIR" \
MUMPS_INCLUDE_DIR="$MUMPS_INCLUDE_DIR" \
LINK_MODE="$LINK_MODE" \
VOLCA_OPT_LEVEL="$OPT_LEVEL" \
./gen-cabal-config.sh

# If --no-optimize was requested but a previous build left a UPX'd binary
# in dist-newstyle, delete it so cabal re-links a clean (decompressible)
# copy. cabal-install caches the link product and would otherwise re-use
# the compressed one as-is.
if [[ "$SKIP_OPTIMIZE" == "true" ]]; then
    PREV_BIN=$(cabal list-bin exe:volca 2>/dev/null || true)
    if [[ -n "$PREV_BIN" && -f "$PREV_BIN" ]] && upx -t "$PREV_BIN" &>/dev/null; then
        log_info "--no-optimize: removing previously UPX'd binary to force re-link"
        rm -f "$PREV_BIN"
    fi
fi

cabal build -j "${STRICT_OPTS[@]}"

VOLCA_BIN_PATH=$(cabal list-bin exe:volca 2>/dev/null || true)
if [[ -n "$VOLCA_BIN_PATH" && -f "$VOLCA_BIN_PATH" ]]; then
    if [[ "$SKIP_OPTIMIZE" == "true" ]]; then
        FINAL_SIZE=$(du -h "$VOLCA_BIN_PATH" | cut -f1)
        log_info "--no-optimize: leaving binary unstripped/uncompressed ($FINAL_SIZE)"
    else
        optimize_volca_binary "$VOLCA_BIN_PATH"
    fi
fi

log_success "volca built successfully"
echo ""

# -----------------------------------------------------------------------------
# Run tests
# -----------------------------------------------------------------------------

if [[ "$RUN_TESTS" == "true" ]]; then
    log_info "Running tests..."
    cd "$SCRIPT_DIR"
    if [[ "$COVERAGE" == "true" ]]; then
        log_info "Coverage enabled — instrumenting code..."
        cabal test --enable-coverage --test-show-details=streaming "${STRICT_OPTS[@]}"

        log_info "Generating coverage report..."
        TIX_FILE=$(find "$SCRIPT_DIR/dist-newstyle" -name "lca-tests.tix" -path "*/hpc/*" 2>/dev/null | head -1)
        if [[ -n "$TIX_FILE" && -f "$TIX_FILE" ]]; then
            COVERAGE_DIR="$SCRIPT_DIR/coverage-report"
            rm -rf "$COVERAGE_DIR"
            mkdir -p "$COVERAGE_DIR"

            HPC_FLAGS=""
            while IFS= read -r d; do
                HPC_FLAGS="$HPC_FLAGS --hpcdir=$d"
            done < <(find "$SCRIPT_DIR/dist-newstyle" -type d -name "mix" -path "*hpc*" 2>/dev/null)

            if [[ -n "$HPC_FLAGS" ]]; then
                hpc markup "$TIX_FILE" $HPC_FLAGS \
                    --srcdir="$SCRIPT_DIR" \
                    --srcdir="$SCRIPT_DIR/mumps-hs" \
                    --destdir="$COVERAGE_DIR" \
                    --fun-entry-count
                hpc report "$TIX_FILE" $HPC_FLAGS
                log_success "Coverage report: $COVERAGE_DIR/hpc_index.html"
            else
                log_warn "Could not locate HPC mix files"
            fi
        else
            log_warn "No .tix file found — coverage report not generated"
        fi
    else
        # VOLCA_TEST_OPTS forwards args to the Hspec executable. CI uses it
        # to set a per-spec timeout (--timeout=300) so a hang becomes a
        # readable failure instead of an indefinite stall.
        EXTRA_TEST_OPTS=()
        if [[ -n "${VOLCA_TEST_OPTS:-}" ]]; then
            EXTRA_TEST_OPTS=(--test-options="$VOLCA_TEST_OPTS")
        fi
        # Resolve the volca exe path now and export it so test/ServerSpec.hs
        # does not spawn its own `cabal list-bin` from inside `cabal test` —
        # that re-config attempt deadlocks against the parent's project lock
        # on Windows and the run hangs until the runner timeout.
        if [[ -z "${VOLCA_EXE:-}" ]]; then
            VOLCA_EXE_RESOLVED=$(cabal list-bin exe:volca 2>/dev/null || true)
            if [[ -n "$VOLCA_EXE_RESOLVED" && -f "$VOLCA_EXE_RESOLVED" ]]; then
                export VOLCA_EXE="$VOLCA_EXE_RESOLVED"
            fi
        fi
        cabal test --test-show-details=streaming "${EXTRA_TEST_OPTS[@]}" "${STRICT_OPTS[@]}"
    fi
    log_success "Tests passed"
    echo ""
fi

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

echo ""
echo "============================================================================="
log_success "Build completed successfully!"
echo "============================================================================="
echo ""

if [[ "$STATIC_BUILD" == "true" ]]; then
    echo "Static build — no LD_LIBRARY_PATH needed."
fi
echo ""
echo "To run volca:"
echo ""
echo "  cabal run volca -- --help"
echo ""

# Find the built executable
VOLCA_BIN=$(cabal list-bin exe:volca 2>/dev/null || true)
if [[ -n "$VOLCA_BIN" ]]; then
    echo "Executable: $VOLCA_BIN"
    if [[ "$STATIC_BUILD" == "true" ]]; then
        echo ""
        echo "Verify static linking with: ldd $VOLCA_BIN"
        echo "(should show no MUMPS entries)"
    fi
    echo ""
fi

