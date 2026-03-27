#!/bin/bash
# Build the volca Docker image
# Run from the volca directory
#
# Usage:
#   ./docker-build.sh [--config /path/to/volca.toml]
#
# When --config is provided, all databases and methods referenced in the TOML
# are bundled into the image with auto-load at startup.

set -e

cd "$(dirname "$0")"
source lib.sh

CUSTOM_CONFIG=""
while [[ $# -gt 0 ]]; do
    case $1 in
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
            echo "Usage: $0 [--config /path/to/volca.toml]"
            exit 1
            ;;
    esac
done

GIT_HASH=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")
if ! git diff --quiet HEAD 2>/dev/null; then
    GIT_HASH="${GIT_HASH}-dirty"
fi
GIT_TAG=$(git describe --tags --exact-match HEAD 2>/dev/null || echo "")

# Stage .docker-bundle/ — Dockerfile always COPYs from here.
BUNDLE_DIR=".docker-bundle"
rm -rf "$BUNDLE_DIR"
mkdir -p "$BUNDLE_DIR/data"

if [[ -n "$CUSTOM_CONFIG" ]]; then
    log_info "Bundling databases from config: $CUSTOM_CONFIG"
    bundle_databases_from_config "$CUSTOM_CONFIG" "$BUNDLE_DIR/data" "$BUNDLE_DIR/volca.toml"

    # Rewrite paths for Docker's /app/data/ location
    sed -i 's|"data/|"/app/data/|g' "$BUNDLE_DIR/volca.toml"
else
    # Default: minimal config for Docker (bind on all interfaces, no pre-loaded databases)
    cat > "$BUNDLE_DIR/volca.toml" <<'EOF'
[server]
port = 8080
host = "0.0.0.0"
EOF
fi

# Always include reference data
cp data/flows.csv "$BUNDLE_DIR/data/" 2>/dev/null || true
cp data/compartments.csv "$BUNDLE_DIR/data/" 2>/dev/null || true
cp data/units.csv "$BUNDLE_DIR/data/" 2>/dev/null || true

echo "Building Docker image: hash=$GIT_HASH tag=${GIT_TAG:-none}"

docker build \
    --build-arg GIT_HASH="$GIT_HASH" \
    --build-arg GIT_TAG="$GIT_TAG" \
    -t volca .

# Clean up staging directory
rm -rf "$BUNDLE_DIR"
