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

# Check tool version: fail only on major version mismatch, warn otherwise
# Usage: check_version_major tool_name actual_version expected_version
check_version_major() {
    local tool="$1"
    local actual="$2"
    local expected="$3"
    local actual_major="${actual%%.*}"
    local expected_major="${expected%%.*}"

    if [[ "$actual" == "$expected" ]]; then
        log_success "$tool version $actual"
        return 0
    elif [[ "$actual_major" == "$expected_major" ]]; then
        log_warn "$tool version $actual (expected $expected)"
        return 0
    else
        log_error "$tool major version $actual_major (expected $expected_major)"
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

# Bundle databases referenced in a TOML config file.
# Copies each database file to target_data_dir and rewrites paths in the config.
# Also copies method files if present.
#
# Usage: bundle_databases_from_config config_file target_data_dir output_config
bundle_databases_from_config() {
    local config_file="$1"
    local target_data_dir="$2"
    local output_config="$3"

    mkdir -p "$target_data_dir"

    # Resolve paths relative to the config file's directory
    local config_dir
    config_dir="$(cd "$(dirname "$config_file")" && pwd)"

    # Use awk to extract paths from [[databases]] and [[methods]] sections,
    # then rewrite them to point to the bundled data/ directory.
    # We track which TOML section we're in and grab the path field.
    local paths
    paths=$(awk '
        /^\[\[databases\]\]/ { section = "databases"; next }
        /^\[\[methods\]\]/   { section = "methods"; next }
        /^\[\[/              { section = ""; next }
        /^\[/                { section = ""; next }
        section != "" && /^path\s*=/ {
            # Extract the quoted path value (2-arg match for mawk compatibility)
            if (match($0, /"[^"]+"/) > 0)
                print substr($0, RSTART+1, RLENGTH-2)
        }
    ' "$config_file")

    if [[ -z "$paths" ]]; then
        log_warn "No database or method paths found in $config_file"
        cp "$config_file" "$output_config"
        return 0
    fi

    # Copy each referenced file and build sed substitutions
    local sed_args=()
    local count=0
    while IFS= read -r db_path; do
        [[ -z "$db_path" ]] && continue

        # Resolve relative paths against config file directory
        local abs_path
        if [[ "$db_path" = /* ]]; then
            abs_path="$db_path"
        else
            abs_path="$config_dir/$db_path"
        fi

        local basename
        basename="$(basename "$abs_path")"

        if [[ -f "$abs_path" ]]; then
            cp "$abs_path" "$target_data_dir/$basename"
            log_success "Bundled: $basename ($(du -h "$abs_path" | cut -f1))"
            count=$((count + 1))
        else
            log_error "Database file not found: $abs_path"
            return 1
        fi

        # Escape special characters in path for sed
        local escaped_path
        escaped_path=$(printf '%s\n' "$db_path" | sed 's/[&/\]/\\&/g')
        sed_args+=(-e "s|\"${escaped_path}\"|\"data/${basename}\"|g")
    done <<< "$paths"

    # Rewrite paths in the config
    if [[ ${#sed_args[@]} -gt 0 ]]; then
        sed "${sed_args[@]}" "$config_file" > "$output_config"
    else
        cp "$config_file" "$output_config"
    fi

    log_success "Bundled $count database/method file(s)"
}
