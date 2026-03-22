#!/bin/bash

# =============================================================================
# volca build script with PETSc
# =============================================================================
# This script builds volca with PETSc integration.
# It can automatically download and build PETSc if not found.
# Works on Linux, macOS, and Windows (via MSYS2).
#
# Usage:
#   ./build.sh [options]
#
# Options:
#   --help              Show this help message
#   --clean             Clean build artifacts before building
#   --all               Force re-download and rebuild of PETSc
#   --test              Run tests after building
#   --coverage          Run tests with coverage and generate HTML report (implies --test)
#   --static            Build a statically-linked binary
#   --desktop           Build desktop application (Tauri bundle, release)
#   --desktop-dev       Build desktop application (debug, no LTO — faster)
#
# Environment variables:
#   PETSC_DIR           Path to PETSc installation
#   PETSC_ARCH          PETSc architecture (default: auto-detected)
#
# Examples:
#   ./build.sh                      # Download deps if needed and build
#   ./build.sh --all                # Force re-download everything
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

# Source PETSc configuration
if [[ -f "$SCRIPT_DIR/petsc.env" ]]; then
    # shellcheck source=petsc.env
    source "$SCRIPT_DIR/petsc.env"
else
    log_error "petsc.env not found in $SCRIPT_DIR"
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

# Set default PETSC_ARCH based on OS
# Auto-detect existing PETSC_ARCH if not set
if [[ -z "$PETSC_ARCH" ]]; then
    PETSC_DIR_CHECK=${PETSC_DIR:-"$SCRIPT_DIR/petsc"}
    PETSC_ARCH=$(detect_existing_petsc_arch "$PETSC_DIR_CHECK")
fi

echo ""
log_info "Build configuration:"
log_info "  OS: $OS"
log_info "  PETSC_ARCH: $PETSC_ARCH"
if [[ "$STATIC_BUILD" == "true" ]]; then
    log_info "  Static linking: enabled"
fi
echo ""

# Check MSYS2 environment on Windows
if [[ "$OS" == "windows" ]]; then
    if ! require_msys2_ucrt64; then
        exit 1
    fi
    # PETSc requires cygwin/msys python, not mingw python (which reports win32)
    PYTHON=/usr/bin/python3
else
    PYTHON=python3
fi

# -----------------------------------------------------------------------------
# Check dependencies
# -----------------------------------------------------------------------------

log_info "Checking dependencies..."

MISSING_DEPS=false

# Required build tools (platform-specific)
if [[ "$OS" == "windows" ]]; then
    # On Windows/MSYS2, check for MinGW tools
    for cmd in gcc make cmake python3 curl tar git; do
        if ! check_command "$cmd"; then
            MISSING_DEPS=true
        fi
    done
else
    # Linux/macOS
    for cmd in gcc g++ make python3 curl tar; do
        if ! check_command "$cmd"; then
            MISSING_DEPS=true
        fi
    done
fi

# Fortran compiler (needed for PETSc)
if ! check_command "gfortran"; then
    log_warn "gfortran not found - PETSc build may fail"
    if [[ "$OS" == "windows" ]]; then
        log_warn "Install with: pacman -S mingw-w64-ucrt-x86_64-gcc-fortran"
    else
        log_warn "Install with: sudo apt install gfortran"
    fi
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
        echo "  pacman -S mingw-w64-ucrt-x86_64-gcc mingw-w64-ucrt-x86_64-gcc-fortran \\"
        echo "            mingw-w64-ucrt-x86_64-msmpi mingw-w64-ucrt-x86_64-openblas make python git"
        echo ""
    elif [[ "$OS" == "macos" ]]; then
        echo "On macOS:"
        echo "  brew install gcc python3 curl node"
        echo ""
    else
        echo "On Debian/Ubuntu:"
        echo "  sudo apt install build-essential python3 curl gfortran liblapack-dev libblas-dev nodejs npm"
        echo ""
        echo "On Fedora:"
        echo "  sudo dnf install gcc gcc-c++ gcc-gfortran make python3 curl lapack-devel blas-devel nodejs npm"
        echo ""
        echo "On Arch Linux:"
        echo "  sudo pacman -S base-devel python curl gcc-fortran lapack blas nodejs npm"
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
# Locate or download PETSc
# -----------------------------------------------------------------------------

# Check for system PETSc (Debian/Ubuntu packages) - Linux only
detect_system_petsc() {
    local petsc_base="/usr/lib/petscdir"
    if [[ -d "$petsc_base" ]]; then
        local latest
        latest=$(ls -d "$petsc_base"/petsc*/x86_64-linux-gnu-real 2>/dev/null | sort -V | tail -1)
        if [[ -n "$latest" && -d "$latest" ]]; then
            echo "$latest"
            return 0
        fi
    fi
    return 1
}

# Check for system packages first (Linux only)
USE_SYSTEM_PETSC=false
SYSTEM_PETSC_DIR=""

if [[ "$OS" == "linux" ]]; then
    if SYSTEM_PETSC_DIR=$(detect_system_petsc); then
        if [[ "$FORCE_REBUILD" != "true" ]]; then
            USE_SYSTEM_PETSC=true
            log_success "Found system PETSc: $SYSTEM_PETSC_DIR"

            SYSTEM_PETSC_VERSION=$(echo "$SYSTEM_PETSC_DIR" | grep -oP 'petsc\K[0-9.]+')
            log_info "System PETSc version: $SYSTEM_PETSC_VERSION"
        else
            log_info "System PETSc found but --all forces rebuild from source"
        fi
    fi
fi

# Default paths (used when building from source)
PETSC_DIR=${PETSC_DIR:-"$SCRIPT_DIR/petsc"}

# Get platform-specific configure options
get_petsc_configure_platform() {
    case "$OS" in
        linux)  echo "$PETSC_CONFIGURE_LINUX" ;;
        macos)  echo "$PETSC_CONFIGURE_MACOS" ;;
        windows) echo "$PETSC_CONFIGURE_WINDOWS" ;;
        *)      echo "$PETSC_CONFIGURE_LINUX" ;;
    esac
}

download_and_build_petsc() {
    log_info "Downloading and building PETSc $PETSC_VERSION..."
    echo ""

    cd "$SCRIPT_DIR"

    # Windows-specific extra configure args
    local EXTRA_ARGS=""
    if [[ "$OS" == "windows" ]]; then
        if [[ ! -f "/ucrt64/lib/libmsmpi.dll.a" ]]; then
            log_error "MSYS2 msmpi package not found"
            log_error "Install with: pacman -S mingw-w64-ucrt-x86_64-msmpi"
            exit 1
        fi
        local MSMPI_BIN="/c/Program Files/Microsoft MPI/Bin"
        if [[ ! -f "$MSMPI_BIN/mpiexec.exe" ]]; then
            log_warn "MS-MPI runtime not found at $MSMPI_BIN"
            log_warn "Install MS-MPI runtime from https://github.com/microsoft/Microsoft-MPI/releases"
        fi
        EXTRA_ARGS="--with-cc=/ucrt64/bin/mpicc.exe --with-fc=/ucrt64/bin/mpifort.exe"
        if [[ -f "$MSMPI_BIN/mpiexec.exe" ]]; then
            local MSMPI_BIN_SHORT
            MSMPI_BIN_SHORT=$(cd "$MSMPI_BIN" 2>/dev/null && pwd || echo "$MSMPI_BIN")
            EXTRA_ARGS="$EXTRA_ARGS --with-mpiexec=$MSMPI_BIN_SHORT/mpiexec"
        fi
        log_info "Using MSYS2 msmpi package with MS-MPI runtime"
    fi

    PETSC_DIR="$SCRIPT_DIR/petsc" \
    PETSC_PLATFORM_OPTS="$(get_petsc_configure_platform)" \
    EXTRA_CONFIGURE_ARGS="$EXTRA_ARGS" \
    PYTHON="$PYTHON" \
    ./build-petsc.sh

    PETSC_DIR="$SCRIPT_DIR/petsc"
    log_success "PETSc built successfully"
    echo ""
}

# Clone petsc-hs if needed
PETSC_HS_DIR="$SCRIPT_DIR/petsc-hs"
if [[ ! -d "$PETSC_HS_DIR" ]] || [[ -z "$(ls -A "$PETSC_HS_DIR" 2>/dev/null)" ]]; then
    log_info "Initializing petsc-hs submodule..."
    git -C "$SCRIPT_DIR" submodule update --init petsc-hs
elif [[ "$FORCE_REBUILD" == "true" ]]; then
    log_info "Cleaning petsc-hs build artifacts..."
    rm -rf "$PETSC_HS_DIR/dist-newstyle"
fi

# Generate TypesC2HsGen.hs if it doesn't exist
TYPES_C2HS_GEN="$PETSC_HS_DIR/src/Numerical/PETSc/Internal/C2HsGen/TypesC2HsGen.hs"
if [[ ! -f "$TYPES_C2HS_GEN" ]]; then
    log_info "Generating TypesC2HsGen.hs..."
    runhaskell "$PETSC_HS_DIR/src/Numerical/PETSc/Internal/C2HsGen/GenerateC2Hs.hs" > "$TYPES_C2HS_GEN"
fi


# Use system packages or build from source
if [[ "$USE_SYSTEM_PETSC" == "true" ]]; then
    # Use system packages
    PETSC_DIR="$SYSTEM_PETSC_DIR"
    PETSC_ARCH=""  # System packages don't use PETSC_ARCH
    USE_SYSTEM_LIBS=true

    # Debian uses libpetsc_real.so
    PETSC_LIB_NAME="petsc_real"

    log_success "Using system PETSc: $PETSC_DIR"
else
    USE_SYSTEM_LIBS=false
    PETSC_LIB_NAME="petsc"

    # Download/build PETSc if needed or forced
    if [[ "$FORCE_REBUILD" == "true" ]] || [[ ! -d "$PETSC_DIR/$PETSC_ARCH" ]]; then
        if [[ "$FORCE_REBUILD" == "true" && -d "$PETSC_DIR" ]]; then
            log_info "Removing existing PETSc for rebuild..."
            rm -rf "$PETSC_DIR" petsc-*.tar.gz petsc-[0-9]*
        fi
        download_and_build_petsc
    fi

    log_success "Using PETSc: $PETSC_DIR/$PETSC_ARCH"
fi
echo ""

# -----------------------------------------------------------------------------
# Set up environment
# -----------------------------------------------------------------------------

if [[ "$USE_SYSTEM_LIBS" == "true" ]]; then
    PETSC_LIB_DIR="/usr/lib/x86_64-linux-gnu"
    PETSC_INCLUDE_DIR="$PETSC_DIR/include"
else
    PETSC_LIB_DIR="$PETSC_DIR/$PETSC_ARCH/lib"
    PETSC_INCLUDE_DIR="$PETSC_DIR/include"
    PETSC_ARCH_INCLUDE_DIR="$PETSC_DIR/$PETSC_ARCH/include"
fi

export LD_LIBRARY_PATH="$PETSC_LIB_DIR${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

if [[ "$USE_SYSTEM_LIBS" == "true" ]]; then
    export C_INCLUDE_PATH="$PETSC_INCLUDE_DIR${C_INCLUDE_PATH:+:$C_INCLUDE_PATH}"
    export CPLUS_INCLUDE_PATH="$PETSC_INCLUDE_DIR${CPLUS_INCLUDE_PATH:+:$CPLUS_INCLUDE_PATH}"
else
    export C_INCLUDE_PATH="$PETSC_INCLUDE_DIR:$PETSC_ARCH_INCLUDE_DIR${C_INCLUDE_PATH:+:$C_INCLUDE_PATH}"
    export CPLUS_INCLUDE_PATH="$PETSC_INCLUDE_DIR:$PETSC_ARCH_INCLUDE_DIR${CPLUS_INCLUDE_PATH:+:$CPLUS_INCLUDE_PATH}"
fi

export PETSC_DIR
export PETSC_ARCH

# On Windows, add MSYS2 bin dir to PATH for DLL discovery
if [[ "$OS" == "windows" ]]; then
    # Resolve real MSYS2 bin path (native Windows processes can't use /ucrt64 mount)
    MSYS2_BIN_WIN="$(dirname "$(command -v gcc)")"
    export PATH="$PETSC_LIB_DIR:$MSYS2_BIN_WIN:$PATH"
    export PKG_CONFIG_PATH="$MSYS2_BIN_WIN/../lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
fi

# -----------------------------------------------------------------------------
# Clean if requested
# -----------------------------------------------------------------------------

if [[ "$CLEAN_BUILD" == "true" ]]; then
    log_info "Cleaning build artifacts..."
    cd "$SCRIPT_DIR"
    rm -rf dist-newstyle cabal.project.local
    if [[ -d "$SCRIPT_DIR/petsc-hs" ]]; then
        cd "$SCRIPT_DIR/petsc-hs"
        rm -rf dist-newstyle cabal.project.local
    fi
    log_success "Clean complete"
    echo ""
fi

# -----------------------------------------------------------------------------
# Build volca (petsc-hs is built automatically as a dependency)
# -----------------------------------------------------------------------------

log_info "Building volca..."
cd "$SCRIPT_DIR"

# Generate Version.hs with build info
BUILD_TARGET="$OS" ./gen-version.sh

# Write cabal.project.local with library paths
if [[ "$USE_SYSTEM_LIBS" == "true" ]]; then
    LINK_MODE="system"
elif [[ "$OS" == "windows" ]]; then
    LINK_MODE="windows"
    MSYS2_LIB_DIR="C:/msys64/ucrt64/lib"
    GCC_LIB_DIR=$(find /ucrt64/lib/gcc/x86_64-w64-mingw32 -maxdepth 1 -type d 2>/dev/null | sort -V | tail -1)
    GCC_LIB_DIR="C:/msys64${GCC_LIB_DIR}"
    MPI_INCLUDE_DIR="$SCRIPT_DIR/petsc/arch-msys2-c-opt/include/mpi-headers"
    mkdir -p "$MPI_INCLUDE_DIR"
    cp -f /ucrt64/include/mpi.h /ucrt64/include/mpif.h /ucrt64/include/mpifptr.h "$MPI_INCLUDE_DIR/" 2>/dev/null || true
    # Convert MSYS2 paths to Windows paths for GHC's lld
    win_path() { echo "$1" | sed 's|^/\([a-zA-Z]\)/|\1:/|'; }
    export PETSC_LIB_DIR=$(win_path "$PETSC_LIB_DIR")
    export PETSC_INCLUDE_DIR=$(win_path "$PETSC_INCLUDE_DIR")
    export PETSC_ARCH_INCLUDE_DIR=$(win_path "$PETSC_ARCH_INCLUDE_DIR")
    export MPI_INCLUDE_DIR=$(win_path "$MPI_INCLUDE_DIR")
    export MSYS2_LIB_DIR GCC_LIB_DIR
else
    LINK_MODE="static"
fi

PETSC_LIB_DIR="$PETSC_LIB_DIR" \
PETSC_INCLUDE_DIR="$PETSC_INCLUDE_DIR" \
PETSC_ARCH_INCLUDE_DIR="$PETSC_ARCH_INCLUDE_DIR" \
LINK_MODE="$LINK_MODE" \
./gen-cabal-config.sh

if [[ "$OS" == "windows" ]]; then
    # Copy OpenBLAS DLLs for GHC to find
    log_info "Copying OpenBLAS DLLs for GHC..."
    for dll in $WINDOWS_RUNTIME_DLLS; do
        src="/ucrt64/bin/$dll"
        if [[ -f "$src" ]]; then
            cp "$src" "$SCRIPT_DIR/"
        fi
    done
fi

if [[ "$USE_SYSTEM_LIBS" == "true" ]]; then
    log_info "Using system library: $PETSC_LIB_NAME"
fi

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
                log_success "Binary optimized: $ORIGINAL_SIZE → $STRIPPED_SIZE (stripped) → $FINAL_SIZE (compressed)"
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
        # Find .tix file (cabal places it inside dist-newstyle)
        TIX_FILE=$(find "$SCRIPT_DIR/dist-newstyle" -name "acv-tests.tix" -path "*/hpc/*" 2>/dev/null | head -1)
        if [[ -n "$TIX_FILE" && -f "$TIX_FILE" ]]; then
            COVERAGE_DIR="$SCRIPT_DIR/coverage-report"
            rm -rf "$COVERAGE_DIR"
            mkdir -p "$COVERAGE_DIR"

            # Collect all mix directories (platform/GHC-version dependent paths)
            HPC_FLAGS=""
            while IFS= read -r d; do
                HPC_FLAGS="$HPC_FLAGS --hpcdir=$d"
            done < <(find "$SCRIPT_DIR/dist-newstyle" -type d -name "mix" -path "*hpc*" 2>/dev/null)

            if [[ -n "$HPC_FLAGS" ]]; then
                hpc markup "$TIX_FILE" $HPC_FLAGS \
                    --srcdir="$SCRIPT_DIR" \
                    --srcdir="$SCRIPT_DIR/petsc-hs" \
                    --destdir="$COVERAGE_DIR" \
                    --fun-entry-count
                hpc report "$TIX_FILE" $HPC_FLAGS
                log_success "Coverage report: $COVERAGE_DIR/hpc_index.html"
                echo ""
                echo "To publish to GitHub Pages:"
                echo "  cd $COVERAGE_DIR"
                echo "  git init && git checkout -b gh-pages"
                echo "  git add . && git commit -m 'Update coverage report'"
                echo "  git push -f git@github.com:ccomb/volca.git gh-pages"
                echo ""
                echo "Then enable Pages in repo Settings > Pages > Source: gh-pages"
            else
                log_warn "Could not locate HPC mix files"
                log_warn "Try manually: hpc report $TIX_FILE"
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
elif [[ "$OS" == "windows" ]]; then
    echo "To run volca, first ensure the DLLs are in PATH:"
    echo ""
    echo "  export PATH=\"$PETSC_LIB_DIR:/ucrt64/bin:\$PATH\""
else
    echo "To run volca, first set up the library path:"
    echo ""
    echo "  export LD_LIBRARY_PATH=\"$PETSC_DIR/$PETSC_ARCH/lib:\$LD_LIBRARY_PATH\""
fi
echo ""
echo "Then run:"
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
        echo "(should show no PETSc/MUMPS/MPI entries)"
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
