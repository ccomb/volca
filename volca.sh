#!/bin/bash

# Run the volca executable with all arguments forwarded

set -e

BINARY=$(cabal list-bin -O2 volca 2>/dev/null || echo "")
if [[ -z "$BINARY" ]] || [[ ! -x "$BINARY" ]]; then
    echo "ERROR: Could not find volca executable"
    exit 1
fi

"$BINARY" "$@"
