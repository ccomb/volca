#!/bin/bash
# =============================================================================
# volca release script
# =============================================================================
# Creates a release by:
#   1. Setting the release version in volca.cabal and tauri.conf.json
#   2. Committing and tagging
#   3. Bumping to the next -dev version
#   4. Pushing everything
#
# Usage:
#   ./release.sh 0.6.0          # Release v0.6.0, bump to 0.7.0-dev
#   ./release.sh 0.6.0 0.6.1    # Release v0.6.0, bump to 0.6.1-dev
#   ./release.sh --dry-run 0.6.0 # Show what would happen without doing it
#
# =============================================================================

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CABAL_FILE="$SCRIPT_DIR/volca.cabal"
TAURI_CONF="$SCRIPT_DIR/desktop/tauri.conf.json"

DRY_RUN=false

# Parse options
while [[ $# -gt 0 ]]; do
    case $1 in
        --dry-run|-n)
            DRY_RUN=true
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [--dry-run] <version> [next-version]"
            echo ""
            echo "  <version>       Release version (e.g. 0.6.0)"
            echo "  [next-version]  Next dev version (default: bump minor)"
            echo "  --dry-run, -n   Show what would happen"
            echo ""
            echo "Examples:"
            echo "  $0 0.6.0          # Release v0.6.0, bump to 0.7.0-dev"
            echo "  $0 0.6.0 0.6.1    # Release v0.6.0, bump to 0.6.1-dev"
            exit 0
            ;;
        *)
            break
            ;;
    esac
done

VERSION="${1:-}"
NEXT_VERSION="${2:-}"

if [[ -z "$VERSION" ]]; then
    echo "Error: version argument required"
    echo "Usage: $0 [--dry-run] <version> [next-version]"
    exit 1
fi

# Validate version format (digits and dots only)
if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
    echo "Error: version must be in format X.Y.Z (e.g. 0.6.0)"
    exit 1
fi

# Compute next version if not provided: bump minor
if [[ -z "$NEXT_VERSION" ]]; then
    IFS='.' read -r major minor patch <<< "$VERSION"
    NEXT_VERSION="$major.$((minor + 1)).0"
fi

TAG="v$VERSION"

# Check preconditions
if [[ -n "$(git status --porcelain -- "$CABAL_FILE" "$TAURI_CONF")" ]]; then
    echo "Error: $CABAL_FILE or $TAURI_CONF have uncommitted changes"
    exit 1
fi

if git rev-parse "$TAG" >/dev/null 2>&1; then
    echo "Error: tag $TAG already exists"
    exit 1
fi

CHANGELOG="$SCRIPT_DIR/CHANGELOG.md"
if ! grep -q "## \[$VERSION\]" "$CHANGELOG"; then
    echo "Error: CHANGELOG.md has no entry for [$VERSION]"
    echo "Please add a ## [$VERSION] section to CHANGELOG.md before releasing."
    exit 1
fi

echo "Release plan:"
echo "  Version:      $VERSION"
echo "  Tag:          $TAG"
echo "  Next version: ${NEXT_VERSION}-dev"
echo ""

if [[ "$DRY_RUN" == "true" ]]; then
    echo "(dry run — no changes made)"
    exit 0
fi

# --- Set release version ---
set_version() {
    local ver="$1"
    sed -i "s/^version:.*$/version:             $ver/" "$CABAL_FILE"
    sed -i "s/\"version\": \"[^\"]*\"/\"version\": \"$ver\"/" "$TAURI_CONF"
}

set_version "$VERSION"
git add "$CABAL_FILE" "$TAURI_CONF"
git commit -m "Release $TAG"
git tag "$TAG"

# --- Bump to next dev version ---
set_version "${NEXT_VERSION}-dev"
git add "$CABAL_FILE" "$TAURI_CONF"
git commit -m "Bump version to ${NEXT_VERSION}-dev"

# --- Push ---
git push && git push origin "$TAG"

echo ""
echo "Released $TAG and bumped to ${NEXT_VERSION}-dev"
