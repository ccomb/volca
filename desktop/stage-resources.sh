#!/bin/bash
# Stage resources for Tauri build
# This script is called by Tauri's beforeBuildCommand

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TARGET_DIR="$SCRIPT_DIR/target/release"

mkdir -p "$TARGET_DIR/lib" "$TARGET_DIR/web"

# Copy volca binary and config
if [[ -f "$SCRIPT_DIR/resources/volca" ]]; then
    cp "$SCRIPT_DIR/resources/volca" "$TARGET_DIR/"
fi
if [[ -f "$SCRIPT_DIR/resources/volca.toml" ]]; then
    cp "$SCRIPT_DIR/resources/volca.toml" "$TARGET_DIR/"
fi

# Copy libraries
if [[ -d "$SCRIPT_DIR/resources/lib" ]]; then
    cp -r "$SCRIPT_DIR/resources/lib/"* "$TARGET_DIR/lib/" 2>/dev/null || true
fi

# Copy web assets
if [[ -d "$SCRIPT_DIR/resources/web" ]]; then
    cp -r "$SCRIPT_DIR/resources/web/"* "$TARGET_DIR/web/" 2>/dev/null || true
fi

echo "Resources staged to $TARGET_DIR"
