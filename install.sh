#!/bin/sh
# =============================================================================
# volca installer (Linux + macOS)
#
# Usage:
#   curl -fsSL https://raw.githubusercontent.com/ccomb/volca/main/install.sh | sh
#   curl -fsSL https://raw.githubusercontent.com/ccomb/volca/main/install.sh | sh -s -- v0.7.0
#
# What it does:
#   1. Detects platform (linux-amd64, linux-arm64, macos-arm64)
#   2. Downloads volca-<version>-<platform>.tar.gz from the GH Release
#   3. Downloads volca-data-<data-version>.tar.gz (the reference data bundle)
#   4. Verifies both against SHA256SUMS
#   5. Extracts to the OS-native user data dir (matches platformdirs):
#        Linux:   ${XDG_DATA_HOME:-~/.local/share}/volca/<version>/volca
#        macOS:   ~/Library/Application Support/volca/<version>/volca
#      with the data bundle under <root>/data/<data-version>/ and a
#      <root>/data/current symlink pointing at it.
#   6. Installs a thin shim at ~/.local/bin/volca that sets VOLCA_DATA_DIR
#      and execs the real binary. The shim makes the data-bundle layout
#      invisible to users — they just `volca …`.
#
# Override the install root with VOLCA_HOME=/full/path (skips OS detection).
#
# Windows:
#   This script aborts. Use install.ps1 (or download from GH Releases).
# =============================================================================

set -eu

REPO="ccomb/volca"
BIN_DIR="$HOME/.local/bin"
SHIM="$BIN_DIR/volca"

VERSION="${1:-}"

err() { printf 'install.sh: %s\n' "$*" >&2; exit 1; }
say() { printf '==> %s\n' "$*"; }

# --- Platform detection ------------------------------------------------------

UNAME_S=$(uname -s)
UNAME_M=$(uname -m)

case "$UNAME_S" in
    Linux)
        OS=linux
        case "$UNAME_M" in
            x86_64|amd64) ARCH=amd64 ;;
            aarch64|arm64) ARCH=arm64 ;;
            *) err "unsupported Linux arch: $UNAME_M" ;;
        esac
        ;;
    Darwin)
        OS=macos
        case "$UNAME_M" in
            arm64) ARCH=arm64 ;;
            x86_64) err "macOS x86_64 is not built — use macos-arm64 (Apple Silicon)" ;;
            *) err "unsupported macOS arch: $UNAME_M" ;;
        esac
        ;;
    MINGW*|MSYS*|CYGWIN*)
        err "Windows detected — use install.ps1 instead, or download from https://github.com/$REPO/releases"
        ;;
    *)
        err "unsupported OS: $UNAME_S"
        ;;
esac
PLATFORM="${OS}-${ARCH}"

# --- Resolve install root ----------------------------------------------------
# Mirrors platformdirs.user_data_dir("volca", appauthor=False) — the same root
# pyvolca.download() and install.ps1 use, so all three installers share it.

if [ -n "${VOLCA_HOME:-}" ]; then
    SHARE_DIR="$VOLCA_HOME"
else
    case "$OS" in
        linux) SHARE_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/volca" ;;
        macos) SHARE_DIR="$HOME/Library/Application Support/volca" ;;
    esac
fi

# --- Tooling sanity ----------------------------------------------------------

for tool in curl tar; do
    command -v "$tool" >/dev/null || err "missing required tool: $tool"
done

# Pick a SHA256 verifier. Linux ships sha256sum; macOS ships shasum.
if command -v sha256sum >/dev/null; then
    SHA256_CHECK() { sha256sum -c "$1"; }
elif command -v shasum >/dev/null; then
    SHA256_CHECK() { shasum -a 256 -c "$1"; }
else
    err "no SHA256 verifier (sha256sum / shasum) found"
fi

# --- Resolve release tag -----------------------------------------------------

if [ -z "$VERSION" ]; then
    say "Resolving latest release of $REPO"
    VERSION=$(curl -fsSL "https://api.github.com/repos/$REPO/releases/latest" \
        | grep -o '"tag_name": *"[^"]*"' \
        | head -n1 \
        | sed 's/.*"tag_name": *"\([^"]*\)".*/\1/')
    [ -n "$VERSION" ] || err "could not resolve latest release tag"
fi
case "$VERSION" in
    v*) ;;
    *) VERSION="v$VERSION" ;;
esac
PLAIN_VERSION="${VERSION#v}"

say "Installing volca $VERSION for $PLATFORM"

# --- Download SHA256SUMS first; it dictates which assets to fetch -----------

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

REL_BASE="https://github.com/$REPO/releases/download/$VERSION"

say "Downloading SHA256SUMS"
curl -fsSL "$REL_BASE/SHA256SUMS" -o "$WORK/SHA256SUMS"

BIN_ASSET="volca-${PLAIN_VERSION}-${PLATFORM}.tar.gz"
grep -q " $BIN_ASSET\$" "$WORK/SHA256SUMS" \
    || err "release $VERSION has no asset $BIN_ASSET (check https://github.com/$REPO/releases/$VERSION)"

# Find the data bundle line. Format: <sha>  volca-data-<version>.tar.gz
DATA_ASSET=$(awk '{print $2}' "$WORK/SHA256SUMS" | grep '^volca-data-.*\.tar\.gz$' | head -n1)
[ -n "$DATA_ASSET" ] || err "release $VERSION has no volca-data-* asset"
DATA_VERSION=$(printf '%s' "$DATA_ASSET" | sed 's/^volca-data-\(.*\)\.tar\.gz$/\1/')

# --- Download + verify -------------------------------------------------------

say "Downloading $BIN_ASSET"
curl -fsSL "$REL_BASE/$BIN_ASSET" -o "$WORK/$BIN_ASSET"

say "Downloading $DATA_ASSET"
curl -fsSL "$REL_BASE/$DATA_ASSET" -o "$WORK/$DATA_ASSET"

# Verify only the two assets we care about (the SHA256SUMS file lists every
# platform's tarball; restricting the check avoids "FAILED open or read" on
# the platforms we didn't download).
(cd "$WORK" && grep -E " ($BIN_ASSET|$DATA_ASSET)\$" SHA256SUMS > SHA256SUMS.local && SHA256_CHECK SHA256SUMS.local)

# --- Install -----------------------------------------------------------------

mkdir -p "$SHARE_DIR/$PLAIN_VERSION" "$SHARE_DIR/data/$DATA_VERSION" "$BIN_DIR"

say "Extracting binary to $SHARE_DIR/$PLAIN_VERSION"
tar -xzf "$WORK/$BIN_ASSET" -C "$SHARE_DIR/$PLAIN_VERSION"
chmod +x "$SHARE_DIR/$PLAIN_VERSION/volca"

say "Extracting data bundle to $SHARE_DIR/data/$DATA_VERSION"
tar -xzf "$WORK/$DATA_ASSET" -C "$SHARE_DIR/data/$DATA_VERSION"

# Atomic-ish "current" pointer. Symlink swap survives crashes; an in-place
# overwrite would briefly leave the dir partial.
rm -f "$SHARE_DIR/data/current"
ln -s "$DATA_VERSION" "$SHARE_DIR/data/current"

say "Installing shim at $SHIM"
cat > "$SHIM" <<EOF
#!/bin/sh
# Auto-generated by volca install.sh — do not edit. Re-run install.sh
# to regenerate after upgrading.
export VOLCA_DATA_DIR="$SHARE_DIR/data/current"
exec "$SHARE_DIR/$PLAIN_VERSION/volca" "\$@"
EOF
chmod +x "$SHIM"

# --- Post-install guidance ---------------------------------------------------

case ":${PATH:-}:" in
    *":$BIN_DIR:"*) ;;
    *)
        cat <<EOF

NOTE: $BIN_DIR is not on your PATH. Add it (in ~/.bashrc, ~/.zshrc, …):
    export PATH="$BIN_DIR:\$PATH"
EOF
        ;;
esac

cat <<EOF

volca $VERSION installed.
  binary:    $SHARE_DIR/$PLAIN_VERSION/volca
  data:      $SHARE_DIR/data/current  -> $DATA_VERSION
  shim:      $SHIM

Try:    volca --version
EOF
