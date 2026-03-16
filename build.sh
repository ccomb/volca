#!/bin/bash

# =============================================================================
# fplca build script with PETSc
# =============================================================================
# This script builds fplca with PETSc integration.
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

# Source version definitions
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

# Detect OS
OS=$(detect_os)

# Defaults
FORCE_REBUILD=false
RUN_TESTS=false
CLEAN_BUILD=false
BUILD_DESKTOP=false
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
    check_version "Node.js" "$NODE_ACTUAL" "$NODE_VERSION"
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

    local PETSC_URL="${PETSC_URL_BASE}/petsc-$PETSC_VERSION.tar.gz"

    cd "$SCRIPT_DIR"

    if [[ ! -f "petsc-$PETSC_VERSION.tar.gz" ]]; then
        log_info "Downloading PETSc from $PETSC_URL..."
        curl -L -o "petsc-$PETSC_VERSION.tar.gz" "$PETSC_URL"
    else
        log_info "Using cached PETSc tarball"
    fi

    if [[ ! -d "petsc-$PETSC_VERSION" ]]; then
        log_info "Extracting PETSc..."
        tar xzf "petsc-$PETSC_VERSION.tar.gz"
    fi

    # Create symlink (on Windows, use directory copy if symlinks fail)
    if [[ "$OS" == "windows" ]]; then
        rm -rf petsc
        cp -r "petsc-$PETSC_VERSION" petsc
    else
        ln -sfn "petsc-$PETSC_VERSION" petsc
    fi
    PETSC_DIR="$SCRIPT_DIR/petsc"

    cd "$PETSC_DIR"

    # Get platform-specific options
    local PLATFORM_OPTS
    PLATFORM_OPTS=$(get_petsc_configure_platform)

    log_info "Configuring PETSc (optimized, with MUMPS direct solver)..."

    # Build configure arguments
    local -a CONFIGURE_ARGS=($PETSC_CONFIGURE_COMMON $PLATFORM_OPTS)
    CONFIGURE_ARGS+=(COPTFLAGS="$PETSC_COPTFLAGS" FOPTFLAGS="$PETSC_FOPTFLAGS" PETSC_ARCH="$PETSC_ARCH")

    # Windows: use MSYS2 msmpi package (provides GCC-compatible MPI with Fortran bindings)
    if [[ "$OS" == "windows" ]]; then
        # Check for MSYS2 msmpi package
        if [[ ! -f "/ucrt64/lib/libmsmpi.dll.a" ]]; then
            log_error "MSYS2 msmpi package not found"
            log_error "Install with: pacman -S mingw-w64-ucrt-x86_64-msmpi"
            exit 1
        fi

        # Also need MS-MPI runtime installed for mpiexec
        local MSMPI_BIN="/c/Program Files/Microsoft MPI/Bin"
        if [[ ! -f "$MSMPI_BIN/mpiexec.exe" ]]; then
            log_warn "MS-MPI runtime not found at $MSMPI_BIN"
            log_warn "Install MS-MPI runtime from https://github.com/microsoft/Microsoft-MPI/releases"
        fi

        # Use MSYS2 MPI compiler wrappers
        CONFIGURE_ARGS+=("--with-cc=/ucrt64/bin/mpicc.exe" "--with-fc=/ucrt64/bin/mpifort.exe")
        # Tell PETSc where mpiexec is (Windows path with spaces needs DOS short form)
        if [[ -f "$MSMPI_BIN/mpiexec.exe" ]]; then
            local MSMPI_BIN_SHORT
            MSMPI_BIN_SHORT=$(cd "$MSMPI_BIN" 2>/dev/null && pwd || echo "$MSMPI_BIN")
            CONFIGURE_ARGS+=("--with-mpiexec=$MSMPI_BIN_SHORT/mpiexec")
        fi
        log_info "Using MSYS2 msmpi package with MS-MPI runtime"
    fi

    $PYTHON ./configure "${CONFIGURE_ARGS[@]}"

    log_info "Building PETSc..."
    make -j PETSC_DIR="$PETSC_DIR" PETSC_ARCH="$PETSC_ARCH" all

    log_success "PETSc built successfully"
    echo ""
}

# Clone petsc-hs if needed
PETSC_HS_DIR="$SCRIPT_DIR/petsc-hs"
if [[ ! -d "$PETSC_HS_DIR" ]]; then
    log_info "Cloning petsc-hs..."
    git clone https://github.com/ccomb/petsc-hs.git "$PETSC_HS_DIR"
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
# Build fplca (petsc-hs is built automatically as a dependency)
# -----------------------------------------------------------------------------

log_info "Building fplca..."
cd "$SCRIPT_DIR"

# Write cabal.project.local with library paths
if [[ "$USE_SYSTEM_LIBS" == "true" ]]; then
    cat > cabal.project.local << EOF
optimization: 2

extra-lib-dirs: $PETSC_LIB_DIR
extra-include-dirs: $PETSC_INCLUDE_DIR

-- System packages use petsc_real library name and system MPI
package petsc-hs
  ghc-options: -optl-lpetsc_real -optl-lmpi

package fplca
  ghc-options: -optl-lpetsc_real -optl-lmpi
EOF
elif [[ "$OS" == "windows" ]]; then
    # Windows needs additional library paths and linker options
    # GHC uses bundled clang/lld which needs native Windows paths (C:/...)
    # MSYS2 paths like /ucrt64/lib or /c/msys64/... don't work with lld
    MSYS2_LIB_DIR="C:/msys64/ucrt64/lib"
    GCC_LIB_DIR=$(find /ucrt64/lib/gcc/x86_64-w64-mingw32 -maxdepth 1 -type d 2>/dev/null | sort -V | tail -1)
    # Convert to Windows path for lld
    GCC_LIB_DIR="C:/msys64${GCC_LIB_DIR}"

    # Create isolated MPI include dir (full MSYS2 include dir conflicts with GHC's clang)
    MPI_INCLUDE_DIR="$SCRIPT_DIR/petsc/arch-msys2-c-opt/include/mpi-headers"
    mkdir -p "$MPI_INCLUDE_DIR"
    cp -f /ucrt64/include/mpi.h /ucrt64/include/mpif.h /ucrt64/include/mpifptr.h "$MPI_INCLUDE_DIR/" 2>/dev/null || true

    # Convert MSYS2 paths (/c/Users/...) to Windows paths (C:/Users/...) for GHC's lld
    win_path() { echo "$1" | sed 's|^/\([a-zA-Z]\)/|\1:/|'; }
    W_PETSC_LIB_DIR=$(win_path "$PETSC_LIB_DIR")
    W_PETSC_INCLUDE_DIR=$(win_path "$PETSC_INCLUDE_DIR")
    W_PETSC_ARCH_INCLUDE_DIR=$(win_path "$PETSC_ARCH_INCLUDE_DIR")

    cat > cabal.project.local << EOF
optimization: 2

extra-lib-dirs: $W_PETSC_LIB_DIR
              , $MSYS2_LIB_DIR
extra-include-dirs: $W_PETSC_INCLUDE_DIR
                  , $W_PETSC_ARCH_INCLUDE_DIR
                  , $(win_path "$MPI_INCLUDE_DIR")

-- Link MinGW runtime libs and OpenBLAS needed by PETSc (built with UCRT64)
package fplca
  ghc-options: -optl-Wl,--allow-multiple-definition -optl-L$GCC_LIB_DIR -optl-L$MSYS2_LIB_DIR -optl-L$W_PETSC_LIB_DIR -optl-lscalapack -optl-ldmumps -optl-lmumps_common -optl-lpord -optl-l:libmsmpi.dll.a -optl-lopenblas -optl-lgfortran -optl-lgcc -optl-lquadmath -optl-lmingwex -optl-lpthread -optl-lmsvcrt
EOF

    # Copy OpenBLAS DLLs for GHC to find
    log_info "Copying OpenBLAS DLLs for GHC..."
    for dll in $WINDOWS_RUNTIME_DLLS; do
        src="/ucrt64/bin/$dll"
        if [[ -f "$src" ]]; then
            cp "$src" "$SCRIPT_DIR/"
        fi
    done
elif [[ "$STATIC_BUILD" == "true" ]]; then
    # Static linking: embed PETSc and all transitive deps into the binary
    # Uses -Bstatic/-Bdynamic sandwich: PETSc/MUMPS/MPI statically, gfortran/libc dynamically
    # Uses --start-group/--end-group to resolve circular deps between static archives
    log_info "Generating cabal.project.local for static linking..."

    STATIC_LINK_FLAGS="-optl-Wl,-Bstatic -optl-Wl,--start-group -optl-lpetsc -optl-ldmumps -optl-lmumps_common -optl-lpord -optl-lscalapack -optl-lfblas -optl-lflapack -optl-lmpifort -optl-lmpi -optl-Wl,--end-group -optl-Wl,-Bdynamic -optl-lgfortran -optl-lquadmath -optl-lpthread -optl-lm -optl-ldl -optl-lrt"

    cat > cabal.project.local << EOF
optimization: 2

extra-lib-dirs: $PETSC_LIB_DIR
extra-include-dirs: $PETSC_INCLUDE_DIR
                  , $PETSC_ARCH_INCLUDE_DIR

-- Static linking: PETSc + MUMPS + ScaLAPACK + BLAS/LAPACK + MPI (static)
-- gfortran runtime stays dynamic (system libgfortran.a lacks -fPIC)
package petsc-hs
  ghc-options: $STATIC_LINK_FLAGS

package fplca
  ghc-options: $STATIC_LINK_FLAGS
EOF
else
    # Linux/macOS custom build
    # PETSc is built with --with-shared-libraries=0 (static .a files)
    # Static link PETSc/MUMPS/MPI, keep gfortran/libc dynamic
    STATIC_LINK_FLAGS="-optl-Wl,-Bstatic -optl-Wl,--start-group -optl-lpetsc -optl-ldmumps -optl-lmumps_common -optl-lpord -optl-lscalapack -optl-lfblas -optl-lflapack -optl-lmpifort -optl-lmpi -optl-Wl,--end-group -optl-Wl,-Bdynamic -optl-lgfortran -optl-lquadmath -optl-lpthread -optl-lm -optl-ldl -optl-lrt"

    cat > cabal.project.local << EOF
optimization: 2

extra-lib-dirs: $PETSC_LIB_DIR
extra-include-dirs: $PETSC_INCLUDE_DIR
                  , $PETSC_ARCH_INCLUDE_DIR

-- Static linking: PETSc + MUMPS + ScaLAPACK + BLAS/LAPACK + MPI (static)
-- gfortran runtime stays dynamic (system libgfortran.a lacks -fPIC)
package petsc-hs
  ghc-options: $STATIC_LINK_FLAGS

package fplca
  ghc-options: $STATIC_LINK_FLAGS
EOF
fi

if [[ "$USE_SYSTEM_LIBS" == "true" ]]; then
    log_info "Using system library: $PETSC_LIB_NAME"
fi

cabal build -j

log_success "fplca built successfully"
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
    cabal test --test-show-details=streaming
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
    echo "To run fplca, first ensure the DLLs are in PATH:"
    echo ""
    echo "  export PATH=\"$PETSC_LIB_DIR:/ucrt64/bin:\$PATH\""
else
    echo "To run fplca, first set up the library path:"
    echo ""
    echo "  export LD_LIBRARY_PATH=\"$PETSC_DIR/$PETSC_ARCH/lib:\$LD_LIBRARY_PATH\""
fi
echo ""
echo "Then run:"
echo ""
echo "  cabal run fplca -- --help"
echo ""

# Find the built executable
FPLCA_BIN=$(cabal list-bin exe:fplca 2>/dev/null || true)
if [[ -n "$FPLCA_BIN" ]]; then
    echo "Executable: $FPLCA_BIN"
    if [[ "$STATIC_BUILD" == "true" ]]; then
        echo ""
        echo "Verify static linking with: ldd $FPLCA_BIN"
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
