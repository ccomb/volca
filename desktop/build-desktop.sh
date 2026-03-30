#!/bin/bash

# =============================================================================
# VoLCA Desktop Build Script
# =============================================================================
# Builds the desktop application using Tauri, bundling:
# - volca Haskell backend (statically linked, self-contained)
# - Elm frontend
#
# Works on Linux, macOS, and Windows (via MSYS2).
#
# Prerequisites:
#   cargo install tauri-cli --locked
#
# Usage:
#   ./build-desktop.sh [--dev] [--config /path/to/volca.toml]
#
# Options:
#   --dev                Build for development (skips bundling)
#   --config FILE        Bundle databases from this config into the installer
#
# =============================================================================

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# Source shared library functions
if [[ -f "$PROJECT_DIR/lib.sh" ]]; then
    # shellcheck source=../lib.sh
    source "$PROJECT_DIR/lib.sh"
else
    echo "ERROR: lib.sh not found"
    exit 1
fi

# Detect OS
OS=$(detect_os)

# Parse arguments
DEV_MODE=false
CUSTOM_CONFIG=""
while [[ $# -gt 0 ]]; do
    case $1 in
        --dev)
            DEV_MODE=true
            shift
            ;;
        --config)
            CUSTOM_CONFIG="$2"
            if [[ -z "$CUSTOM_CONFIG" || ! -f "$CUSTOM_CONFIG" ]]; then
                log_error "Config file not found: ${CUSTOM_CONFIG:-<empty>}"
                exit 1
            fi
            shift 2
            ;;
        *)
            log_error "Unknown option: $1"
            echo "Usage: $0 [--dev] [--config /path/to/volca.toml]"
            exit 1
            ;;
    esac
done

# -----------------------------------------------------------------------------
# Check prerequisites
# -----------------------------------------------------------------------------

log_info "Checking prerequisites..."

# Check for cargo
if ! command -v cargo &> /dev/null; then
    log_error "cargo not found. Please install Rust: https://rustup.rs/"
    exit 1
fi

# Check for tauri-cli
if ! cargo tauri --version &> /dev/null; then
    log_warn "tauri-cli not found. Installing..."
    cargo install tauri-cli --locked
fi

log_success "Prerequisites OK"
echo ""

# -----------------------------------------------------------------------------
# Detect version from git
# -----------------------------------------------------------------------------

log_info "Detecting version..."

VERSION=$(get_version "$PROJECT_DIR/volca.cabal")
if [[ -z "$VERSION" ]]; then
    VERSION="0.1.0"
fi

log_info "Building version: $VERSION"
echo ""

# -----------------------------------------------------------------------------
# Build backend and frontend
# -----------------------------------------------------------------------------

log_info "Building volca backend and frontend..."
cd "$PROJECT_DIR"

./build.sh

log_success "Backend and frontend built"
echo ""

# -----------------------------------------------------------------------------
# Stage resources
# -----------------------------------------------------------------------------

log_info "Staging resources..."

RESOURCES_DIR="$SCRIPT_DIR/resources"
rm -rf "$RESOURCES_DIR"
mkdir -p "$RESOURCES_DIR/web"

# Copy volca binary
VOLCA_BIN=$(cd "$PROJECT_DIR" && cabal list-bin volca 2>/dev/null)

if [[ -z "$VOLCA_BIN" ]]; then
    log_error "Could not find volca binary"
    exit 1
fi

if [[ "$OS" == "windows" ]]; then
    # On Windows, copy as both volca.exe and volca (for Tauri resource bundling)
    cp "$VOLCA_BIN" "$RESOURCES_DIR/volca.exe"
    cp "$VOLCA_BIN" "$RESOURCES_DIR/volca"
else
    cp "$VOLCA_BIN" "$RESOURCES_DIR/volca"
fi
log_success "Copied volca binary"

# Copy config file (and bundle databases if custom config provided)
if [[ -n "$CUSTOM_CONFIG" ]]; then
    log_info "Bundling databases from custom config: $CUSTOM_CONFIG"
    mkdir -p "$RESOURCES_DIR/data"
    bundle_databases_from_config "$CUSTOM_CONFIG" "$RESOURCES_DIR/data" "$RESOURCES_DIR/volca.toml"
    log_success "Custom config with bundled databases"
else
    cp "$SCRIPT_DIR/volca.toml" "$RESOURCES_DIR/volca.toml"
    log_success "Copied default config (BYOL mode)"
fi

# Copy web assets
if [[ -d "$PROJECT_DIR/web/dist" ]]; then
    cp -r "$PROJECT_DIR/web/dist/"* "$RESOURCES_DIR/web/"
    log_success "Copied web assets"
else
    log_warn "No web/dist directory found - frontend may not be built"
fi

# Copy reference data (flow synonyms, compartment mappings, units)
if [[ -d "$PROJECT_DIR/data" ]]; then
    mkdir -p "$RESOURCES_DIR/data"
    cp "$PROJECT_DIR/data/flows.csv" "$RESOURCES_DIR/data/"
    cp "$PROJECT_DIR/data/compartments.csv" "$RESOURCES_DIR/data/"
    cp "$PROJECT_DIR/data/units.csv" "$RESOURCES_DIR/data/"
    cp "$PROJECT_DIR/data/geographies.csv" "$RESOURCES_DIR/data/"
    log_success "Copied reference data"
else
    log_warn "No data/ directory found - reference data will not be available"
fi

# -----------------------------------------------------------------------------
# Copy 7z binary for archive extraction
# -----------------------------------------------------------------------------

log_info "Staging 7z binary for archive extraction..."

SEVENZIP_STAGED=false

if [[ "$OS" == "windows" ]]; then
    # Windows: Copy from local 7-Zip installation
    SEVENZIP_PATHS=(
        "/c/Program Files/7-Zip/7z.exe"
        "/c/Program Files (x86)/7-Zip/7z.exe"
        "C:/Program Files/7-Zip/7z.exe"
        "C:/Program Files (x86)/7-Zip/7z.exe"
    )

    for sevenzip_path in "${SEVENZIP_PATHS[@]}"; do
        if [[ -f "$sevenzip_path" ]]; then
            sevenzip_dir=$(dirname "$sevenzip_path")
            cp "$sevenzip_path" "$RESOURCES_DIR/7z.exe"
            if [[ -f "$sevenzip_dir/7z.dll" ]]; then
                cp "$sevenzip_dir/7z.dll" "$RESOURCES_DIR/"
            fi
            SEVENZIP_STAGED=true
            log_success "Copied 7z.exe from $sevenzip_dir"
            break
        fi
    done

    if [[ "$SEVENZIP_STAGED" != "true" ]]; then
        log_error "7-Zip not found. Please install 7-Zip from https://7-zip.org/"
        log_error "Expected location: C:\\Program Files\\7-Zip\\7z.exe"
        exit 1
    fi

elif [[ "$OS" == "linux" ]]; then
    # Linux: Download 7-Zip standalone binary
    SEVENZIP_URL="https://7-zip.org/a/7z2408-linux-x64.tar.xz"
    SEVENZIP_TMP="$RESOURCES_DIR/.7z-download"

    # Try to find system 7zz first
    if command -v 7zz &>/dev/null; then
        cp "$(which 7zz)" "$RESOURCES_DIR/7zz"
        chmod +x "$RESOURCES_DIR/7zz"
        SEVENZIP_STAGED=true
        log_success "Copied system 7zz binary"
    elif command -v 7z &>/dev/null; then
        cp "$(which 7z)" "$RESOURCES_DIR/7z"
        chmod +x "$RESOURCES_DIR/7z"
        SEVENZIP_STAGED=true
        log_success "Copied system 7z binary"
    else
        # Download 7-Zip for Linux
        log_info "Downloading 7-Zip for Linux..."
        mkdir -p "$SEVENZIP_TMP"
        if curl -fSL -o "$SEVENZIP_TMP/7z.tar.xz" "$SEVENZIP_URL" 2>/dev/null; then
            tar -xJf "$SEVENZIP_TMP/7z.tar.xz" -C "$SEVENZIP_TMP" 2>/dev/null || true
            if [[ -f "$SEVENZIP_TMP/7zz" ]]; then
                cp "$SEVENZIP_TMP/7zz" "$RESOURCES_DIR/7zz"
                chmod +x "$RESOURCES_DIR/7zz"
                SEVENZIP_STAGED=true
                log_success "Downloaded and staged 7zz binary"
            fi
        fi
        rm -rf "$SEVENZIP_TMP"
    fi

    if [[ "$SEVENZIP_STAGED" != "true" ]]; then
        log_error "Could not stage 7z binary."
        log_error "Please install 7zip: apt install 7zip (or p7zip-full)"
        exit 1
    fi

elif [[ "$OS" == "macos" ]]; then
    # macOS: Use Homebrew's 7zz if available
    if command -v 7zz &>/dev/null; then
        cp "$(which 7zz)" "$RESOURCES_DIR/7zz"
        chmod +x "$RESOURCES_DIR/7zz"
        SEVENZIP_STAGED=true
        log_success "Copied Homebrew 7zz binary"
    elif command -v 7z &>/dev/null; then
        cp "$(which 7z)" "$RESOURCES_DIR/7z"
        chmod +x "$RESOURCES_DIR/7z"
        SEVENZIP_STAGED=true
        log_success "Copied system 7z binary"
    else
        log_error "7-Zip not found. Please install with: brew install p7zip"
        exit 1
    fi
fi

echo ""

# -----------------------------------------------------------------------------
# Verify icons exist
# -----------------------------------------------------------------------------

ICONS_DIR="$SCRIPT_DIR/icons"
if [[ ! -f "$ICONS_DIR/32x32.png" ]] || [[ ! -f "$ICONS_DIR/icon.ico" ]]; then
    log_error "Missing required icons in $ICONS_DIR"
    log_error "Required: 32x32.png, 128x128.png, 128x128@2x.png, icon.ico"
    exit 1
fi

# -----------------------------------------------------------------------------
# Stage resources to target/release for direct binary testing
# -----------------------------------------------------------------------------

log_info "Staging resources to target/release..."
TARGET_DIR="$SCRIPT_DIR/target/release"
mkdir -p "$TARGET_DIR/web"

if [[ "$OS" == "windows" ]]; then
    cp "$RESOURCES_DIR/volca.exe" "$TARGET_DIR/"
    cp "$RESOURCES_DIR/volca" "$TARGET_DIR/"
    cp "$RESOURCES_DIR/7z.exe" "$TARGET_DIR/" 2>/dev/null || true
else
    cp "$RESOURCES_DIR/volca" "$TARGET_DIR/"
    cp "$RESOURCES_DIR/7zz" "$TARGET_DIR/" 2>/dev/null || true
    cp "$RESOURCES_DIR/7z" "$TARGET_DIR/" 2>/dev/null || true
fi

cp "$RESOURCES_DIR/volca.toml" "$TARGET_DIR/" 2>/dev/null || true
cp -r "$RESOURCES_DIR/web/"* "$TARGET_DIR/web/" 2>/dev/null || true
if [[ -d "$RESOURCES_DIR/data" ]]; then
    mkdir -p "$TARGET_DIR/data"
    cp -r "$RESOURCES_DIR/data/"* "$TARGET_DIR/data/"
fi
log_success "Resources staged to target/release"

# -----------------------------------------------------------------------------
# Build Tauri app
# -----------------------------------------------------------------------------

cd "$SCRIPT_DIR"

# Pass version override via --config so we don't modify tracked tauri.conf.json
VERSION_CONFIG="{\"version\":\"$VERSION\"}"
log_info "Building with version $VERSION"
echo ""

if [[ "$DEV_MODE" == "true" ]]; then
    log_info "Building Tauri app (development mode)..."
    cargo tauri build --debug --config "$VERSION_CONFIG"
else
    log_info "Building Tauri app (release mode)..."

    # Platform-specific bundle types
    case "$OS" in
        linux)
            cargo tauri build --bundles deb --config "$VERSION_CONFIG"
            ;;
        macos)
            cargo tauri build --bundles dmg --config "$VERSION_CONFIG"
            ;;
        windows)
            cargo tauri build --bundles nsis --config "$VERSION_CONFIG"
            ;;
        *)
            cargo tauri build --config "$VERSION_CONFIG"
            ;;
    esac
fi

log_success "Tauri build complete"
echo ""

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

echo "============================================================================="
log_success "Desktop build completed!"
echo "============================================================================="
echo ""

# Find generated bundles
if [[ "$DEV_MODE" == "true" ]]; then
    BUNDLE_DIR="$SCRIPT_DIR/target/debug/bundle"
else
    BUNDLE_DIR="$SCRIPT_DIR/target/release/bundle"
fi

if [[ -d "$BUNDLE_DIR" ]]; then
    echo "Generated bundles:"
    case "$OS" in
        linux)
            find "$BUNDLE_DIR" -type f \( -name "*.AppImage" -o -name "*.deb" \) -exec echo "  {}" \;
            ;;
        macos)
            find "$BUNDLE_DIR" -type f -name "*.dmg" -exec echo "  {}" \;
            ;;
        windows)
            find "$BUNDLE_DIR" -type f -name "*.exe" -exec echo "  {}" \;
            ;;
    esac
    echo ""
fi

echo "To run the desktop app in development mode:"
echo "  cd desktop && cargo tauri dev"
echo ""
