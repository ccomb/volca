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
#   --desktop           Build desktop application (Tauri bundle, release)
#   --desktop-dev       Build desktop application (debug, no LTO — faster)
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
BUILD_DESKTOP=false
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
        --desktop)
            BUILD_DESKTOP=true
            shift
            ;;
        --desktop-dev)
            BUILD_DESKTOP=true
            DESKTOP_DEV=true
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

# Node.js (needed for frontend build)
if ! check_command "npm"; then
    MISSING_DEPS=true
    log_warn "Install Node.js: https://nodejs.org/"
fi

# Rust (needed for desktop build)
check_command "rustc"

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

# Linux fallback: build MUMPS from source
if [[ "$MUMPS_FOUND" != "true" ]] && [[ "$OS" == "linux" ]]; then
    log_info "MUMPS not found; building ${MUMPS_VERSION} from source (this may take a few minutes)..."
    MUMPS_VERSION="$MUMPS_VERSION" \
    OUTPUT_DIR="$LOCAL_MUMPS_DIR" \
    BUILD_DIR="$SCRIPT_DIR/deps/build" \
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

# Elm (installed via npm in web/ directory)
if command -v elm &>/dev/null; then
    log_success "elm found: $(command -v elm)"
else
    log_info "elm will be installed via npm in web/"
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
        echo "  sudo apt install build-essential python3 curl zlib1g-dev libmumps-seq-dev nodejs npm"
        echo ""
        echo "On Fedora:"
        echo "  sudo dnf install gcc gcc-c++ make python3 curl zlib-devel MUMPS-devel nodejs npm"
        echo ""
        echo "On Arch Linux:"
        echo "  sudo pacman -S base-devel python curl zlib mumps nodejs npm"
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

# Check Node version
if command -v node &> /dev/null; then
    NODE_ACTUAL=$(node --version | sed 's/^v//')
    check_version_major "Node.js" "$NODE_ACTUAL" "$NODE_VERSION"
fi

# Check Rust version (optional, for desktop build)
if command -v rustc &> /dev/null; then
    RUST_ACTUAL=$(rustc --version | awk '{print $2}')
    check_version "Rust" "$RUST_ACTUAL" "$RUST_VERSION"
fi

# Check Elm version (optional, installed via npm if missing)
if command -v elm &> /dev/null; then
    ELM_ACTUAL=$(elm --version 2>/dev/null || echo "unknown")
    if [[ "$ELM_ACTUAL" != "unknown" ]]; then
        check_version "Elm" "$ELM_ACTUAL" "$ELM_VERSION"
    fi
fi

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

            # UPX compression
            log_info "Compressing with UPX..."
            if upx -1 "$VOLCA_BIN_PATH"; then
                FINAL_SIZE=$(du -h "$VOLCA_BIN_PATH" | cut -f1)
                log_success "Binary optimized: $ORIGINAL_SIZE -> $STRIPPED_SIZE (stripped) -> $FINAL_SIZE (compressed)"
            else
                log_warn "UPX compression failed — using stripped binary ($STRIPPED_SIZE)"
            fi
        fi
    fi
}

log_success "volca built successfully"
echo ""

# -----------------------------------------------------------------------------
# Build frontend
# -----------------------------------------------------------------------------

if [[ -d "$SCRIPT_DIR/web" ]]; then
    log_info "Building frontend..."
    cd "$SCRIPT_DIR/web"
    if [[ ! -d "node_modules" ]]; then
        npm install --silent
    fi
    ./build.sh
    log_success "Frontend built successfully"
    echo ""
fi

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
        TIX_FILE=$(find "$SCRIPT_DIR/dist-newstyle" -name "acv-tests.tix" -path "*/hpc/*" 2>/dev/null | head -1)
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
# Build desktop application (if requested)
# -----------------------------------------------------------------------------

if [[ "$BUILD_DESKTOP" == "true" ]]; then
    log_info "Building desktop application..."
    cd "$SCRIPT_DIR/desktop"
    if [[ "$DESKTOP_DEV" == "true" ]]; then
        ./build-desktop.sh --dev
    else
        ./build-desktop.sh
    fi
    log_success "Desktop application built"
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

# Show desktop build instructions if not already built
if [[ "$BUILD_DESKTOP" != "true" ]]; then
    echo "To build the desktop application:"
    echo ""
    echo "  ./build.sh --desktop"
    echo ""
    echo "Or manually:"
    echo "  cd desktop && ./build-desktop.sh"
    echo ""
fi
