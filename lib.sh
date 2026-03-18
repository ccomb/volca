#!/usr/bin/env bash
# =============================================================================
# Shared functions for VoLCA build scripts
# =============================================================================
# Source this file from build.sh and desktop/build-desktop.sh
#
# Usage:
#   source "$SCRIPT_DIR/scripts/lib.sh"
# =============================================================================

# Colors for output (works on most terminals, including MSYS2)
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[OK]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Platform detection
# Returns: linux, macos, windows, or unknown
detect_os() {
    case "$(uname -s)" in
        Linux*)  echo "linux" ;;
        Darwin*) echo "macos" ;;
        MINGW*|MSYS*|CYGWIN*|*_NT*) echo "windows" ;;
        *)       echo "unknown" ;;
    esac
}

# PETSC_ARCH detection based on OS and debug flag
# Usage: detect_petsc_arch [debug]
# Args:
#   debug: "true" for debug build, anything else for optimized
detect_petsc_arch() {
    local os
    os=$(detect_os)
    local debug="${1:-false}"

    # Windows uses a different naming convention
    if [[ "$os" == "windows" ]]; then
        if [[ "$debug" == "true" ]]; then
            echo "arch-msys2-c-debug"
        else
            echo "arch-msys2-c-opt"
        fi
    else
        if [[ "$debug" == "true" ]]; then
            echo "arch-${os}-c-debug"
        else
            echo "arch-${os}-c-opt"
        fi
    fi
}

# Auto-detect existing PETSC_ARCH from built directories
# Usage: detect_existing_petsc_arch PETSC_DIR
detect_existing_petsc_arch() {
    local petsc_dir="$1"
    local os
    os=$(detect_os)

    # For Windows, check msys2 arch first
    if [[ "$os" == "windows" ]]; then
        if [[ -d "$petsc_dir/arch-msys2-c-opt" ]]; then
            echo "arch-msys2-c-opt"
            return 0
        elif [[ -d "$petsc_dir/arch-msys2-c-debug" ]]; then
            echo "arch-msys2-c-debug"
            return 0
        fi
    fi

    # Standard detection for Linux/macOS
    if [[ -d "$petsc_dir/arch-${os}-c-opt" ]]; then
        echo "arch-${os}-c-opt"
        return 0
    elif [[ -d "$petsc_dir/arch-${os}-c-debug" ]]; then
        echo "arch-${os}-c-debug"
        return 0
    fi

    # Not found - return default
    detect_petsc_arch false
}

# Version detection from git tag or cabal file
# Usage: get_version [cabal_file]
get_version() {
    local cabal_file="${1:-volca.cabal}"

    # Try git tag first
    if git describe --tags --exact-match HEAD 2>/dev/null; then
        git describe --tags --exact-match HEAD | sed 's/^v//'
    else
        # Fall back to cabal file
        grep "^version:" "$cabal_file" 2>/dev/null | awk '{print $2}'
    fi
}

# Check if a command exists
# Usage: check_command command_name [required]
# Args:
#   command_name: The command to check
#   required: "true" (default) to treat as error, "false" for warning
# Returns: 0 if found or optional, 1 only if required and missing
check_command() {
    local cmd="$1"
    local required="${2:-true}"

    if command -v "$cmd" &>/dev/null; then
        log_success "$cmd found: $(command -v "$cmd")"
        return 0
    else
        if [[ "$required" == "true" ]]; then
            log_error "$cmd not found"
            return 1
        else
            log_warn "$cmd not found (optional)"
            return 0  # Don't fail for optional commands
        fi
    fi
}

# Check tool version against expected version (exact match)
# Usage: check_version tool_name actual_version expected_version
check_version() {
    local tool="$1"
    local actual="$2"
    local expected="$3"

    if [[ "$actual" == "$expected" ]]; then
        log_success "$tool version $actual"
        return 0
    else
        log_warn "$tool version $actual (expected $expected)"
        return 1
    fi
}

# Convert Windows path to MSYS2 path
# Usage: to_msys_path "C:\path\to\dir"
to_msys_path() {
    local path="$1"
    # Replace backslashes with forward slashes
    path="${path//\\//}"
    # Replace drive letter (C:) with /c
    echo "$path" | sed 's|^\([A-Za-z]\):|/\L\1|'
}

# Convert MSYS2 path to Windows path
# Usage: to_windows_path "/c/path/to/dir"
to_windows_path() {
    local path="$1"
    # Replace /c with C:
    path=$(echo "$path" | sed 's|^/\([a-zA-Z]\)/|\U\1:/|')
    # Replace forward slashes with backslashes
    echo "${path//\//\\}"
}

# Check if running in MSYS2 environment
is_msys2() {
    [[ -n "${MSYSTEM:-}" ]]
}

# Ensure we're in MSYS2 UCRT64 environment on Windows
require_msys2_ucrt64() {
    if [[ "$(detect_os)" == "windows" ]]; then
        if [[ -z "${MSYSTEM:-}" ]]; then
            log_error "Please run from MSYS2 terminal"
            log_error "Open 'MSYS2 UCRT64' from Start menu"
            return 1
        fi
        if [[ "$MSYSTEM" != "UCRT64" ]]; then
            log_warn "Running in $MSYSTEM environment"
            log_warn "UCRT64 is recommended for best compatibility"
        fi
    fi
    return 0
}

# Show help by extracting header comments from script
# Usage: show_help "$0"
show_help() {
    local script="$1"
    sed -n '3,27p' "$script" | sed 's/^# //' | sed 's/^#//'
    exit 0
}
