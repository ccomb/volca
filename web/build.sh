#!/bin/bash

set -e

echo "Building Elm frontend with asset hashing..."

# Determine version from git
# If HEAD is tagged, use the tag name; otherwise use cabal version
if VERSION=$(git describe --tags --exact-match HEAD 2>/dev/null); then
    VERSION="${VERSION#v}"  # Remove leading 'v' if present
    echo "Building version: $VERSION (from tag)"
else
    VERSION=$(grep "^version:" ../volca.cabal | awk '{print $2}')
    echo "Building version: $VERSION (development)"
fi

# Create dist directory and clean old JS files
mkdir -p dist
rm -f dist/*.js

# Download CSS dependencies if not present
if [ ! -f dist/bulma.min.css ]; then
    echo "Downloading Bulma CSS..."
    curl -sL "https://cdn.jsdelivr.net/npm/bulma@1.0.4/css/bulma.min.css" -o dist/bulma.min.css
fi

if [ ! -f dist/fontawesome.min.css ]; then
    echo "Downloading Font Awesome..."
    curl -sL "https://use.fontawesome.com/releases/v6.0.0/fontawesome-free-6.0.0-web.zip" -o dist/fa.zip
    unzip -q dist/fa.zip -d dist/
    cp dist/fontawesome-free-6.0.0-web/css/all.min.css dist/fontawesome.min.css
    cp -r dist/fontawesome-free-6.0.0-web/webfonts dist/
    # Fix webfont paths in CSS (remove ../ prefix)
    sed -i 's|\.\./webfonts/|webfonts/|g' dist/fontawesome.min.css
    rm -rf dist/fontawesome-free-6.0.0-web dist/fa.zip
fi

# Build Elm to temporary file (use local elm from node_modules)
echo "Compiling Elm..."
npx elm make src/Main.elm --output=dist/main.tmp.js --optimize

# Minify JavaScript with SWC (Elm Guide config for optimal compression)
echo "Minifying with SWC..."
BEFORE_SIZE=$(wc -c < dist/main.tmp.js)
node minify.mjs dist/main.tmp.js dist/main.min.js
mv dist/main.min.js dist/main.tmp.js
AFTER_SIZE=$(wc -c < dist/main.tmp.js)
echo "  $BEFORE_SIZE -> $AFTER_SIZE bytes ($(( (BEFORE_SIZE - AFTER_SIZE) * 100 / BEFORE_SIZE ))% reduction)"

# Calculate MD5 hash of the compiled JS
JS_HASH=$(md5sum dist/main.tmp.js | cut -d' ' -f1)
JS_FILE="$JS_HASH.js"

# Rename the temporary file to hashed name
mv dist/main.tmp.js "dist/$JS_FILE"

# Generate index.html from template
echo "Generating index.html..."
sed -e "s/{{JS_FILE}}/$JS_FILE/g" -e "s/{{VERSION}}/$VERSION/g" index.html.tmpl > dist/index.html

echo "Frontend build complete!"
echo "Generated: dist/$JS_FILE"
echo "Generated: dist/index.html"