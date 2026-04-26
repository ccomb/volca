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
#   --test              Run tests after building
#   --coverage          Run tests with coverage and generate HTML report (implies --test)
#   --static            Build a statically-linked binary
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

# Defaults
FORCE_REBUILD=false
RUN_TESTS=false
CLEAN_BUILD=false
COVERAGE=false
STATIC_BUILD=false

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
        --static)
            STATIC_BUILD=true
            shift
            ;;
        *)
            log_error "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

echo ""
log_info "Build configuration:"
log_info "  OS: $OS"
if [[ "$STATIC_BUILD" == "true" ]]; then
    log_info "  Static linking: enabled"
fi
echo ""

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
    # Linux: check system install
    if [[ -f "/usr/lib/x86_64-linux-gnu/libdmumps_seq.so" ]] || [[ -f "/usr/lib/x86_64-linux-gnu/libdmumps_seq.a" ]]; then
        MUMPS_FOUND=true
        MUMPS_LIB_DIR="/usr/lib/x86_64-linux-gnu"
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
    MSYS2_LIB_DIR="C:/msys64/ucrt64/lib"
    GCC_LIB_DIR=$(find /ucrt64/lib/gcc/x86_64-w64-mingw32 -maxdepth 1 -type d 2>/dev/null | sort -V | tail -1)
    GCC_LIB_DIR="C:/msys64${GCC_LIB_DIR}"
    win_path() { echo "$1" | sed 's|^/\([a-zA-Z]\)/|\1:/|'; }
    export MUMPS_LIB_DIR=$(win_path "$MUMPS_LIB_DIR")
    export MUMPS_INCLUDE_DIR=$(win_path "$MUMPS_INCLUDE_DIR")
    export MSYS2_LIB_DIR GCC_LIB_DIR
elif [[ "$STATIC_BUILD" == "true" ]] || [[ "$MUMPS_BUILT_LOCALLY" == "true" ]]; then
    # Static linking required when using locally-built MUMPS (only .a libs available)
    LINK_MODE="static"
else
    LINK_MODE="dynamic"
fi

MUMPS_LIB_DIR="$MUMPS_LIB_DIR" \
MUMPS_INCLUDE_DIR="$MUMPS_INCLUDE_DIR" \
LINK_MODE="$LINK_MODE" \
./gen-cabal-config.sh

cabal build -j

# Strip and compress the binary
{
    VOLCA_BIN_PATH=$(cabal list-bin exe:volca 2>/dev/null || true)
    if [[ -n "$VOLCA_BIN_PATH" && -f "$VOLCA_BIN_PATH" ]]; then
        # Skip if already UPX-compressed (from a previous build)
        if upx -t "$VOLCA_BIN_PATH" &>/dev/null; then
            FINAL_SIZE=$(du -h "$VOLCA_BIN_PATH" | cut -f1)
            log_info "Binary already optimized ($FINAL_SIZE), skipping strip/UPX"
        else
            ORIGINAL_SIZE=$(du -h "$VOLCA_BIN_PATH" | cut -f1)
            log_info "Binary size before optimization: $ORIGINAL_SIZE"

            # Strip debug symbols
            log_info "Stripping debug symbols..."
            if [[ "$OS" == "macos" ]]; then
                strip -x "$VOLCA_BIN_PATH"
            else
                strip --strip-all "$VOLCA_BIN_PATH"
            fi
            STRIPPED_SIZE=$(du -h "$VOLCA_BIN_PATH" | cut -f1)
            log_info "After strip: $STRIPPED_SIZE"

            # UPX compression (Linux/macOS only — Windows Defender flags UPX binaries
            # as malicious, causing "Error opening file for writing" during NSIS install)
            if [[ "$OS" != "windows" ]]; then
                log_info "Compressing with UPX..."
                if upx -1 "$VOLCA_BIN_PATH"; then
                    FINAL_SIZE=$(du -h "$VOLCA_BIN_PATH" | cut -f1)
                    log_success "Binary optimized: $ORIGINAL_SIZE -> $STRIPPED_SIZE (stripped) -> $FINAL_SIZE (compressed)"
                else
                    log_warn "UPX compression failed — using stripped binary ($STRIPPED_SIZE)"
                fi
            else
                log_success "Binary optimized: $ORIGINAL_SIZE -> $STRIPPED_SIZE (stripped, no UPX on Windows)"
            fi
        fi
    fi
}

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
        cabal test --enable-coverage --test-show-details=streaming

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
        cabal test --test-show-details=streaming
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

