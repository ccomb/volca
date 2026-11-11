#!/bin/sh
# Refuse a THIRD_PARTY_LICENSES.md that disagrees with volca.cabal.
#
# The table is the license inventory of the shipped binary, and /api/v1/licenses
# sends every reader to it through haskell_dependencies_url. It is written by
# hand, so it drifts in both directions: a dependency removed leaves a row
# claiming a library the binary no longer links, and a dependency added leaves
# no row at all. Neither shows up in a build.
#
# Scope is what ships: the direct build-depends of the library and executable
# stanzas. Test and benchmark dependencies never reach a user, and the engine
# itself is not its own dependency.
set -eu

cd "$(dirname "$0")/.."

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

awk '
    # A stanza header sits at column 0. Only the shipped ones count, and a
    # library carries a name once there is more than one of them.
    /^[a-z-]+( |$)/ { ship = ($0 ~ /^library( |$)/ || $0 ~ /^executable /); deps = 0 }
    /^[ \t]*--/     { next }
    {
        line = $0
        if (ship && line ~ /^[ \t]*build-depends:/) {
            deps = 1
            sub(/^[ \t]*build-depends:[ \t]*/, "", line)   # first name shares the line
        } else if (deps && line ~ /^[ \t]*[A-Za-z][A-Za-z0-9-]*:[ \t]*/) {
            deps = 0                                        # the next field ends the list
        }
        if (!deps) next
        gsub(/^[ \t]*,?[ \t]*/, "", line)
        if (line == "") next
        split(line, field, /[ \t]/)                         # drop the version constraint
        if (field[1] != "") print field[1]
    }
' volca.cabal | grep -vx 'volca' | sort -u > "$work/cabal"

# The package column of the table, minus its header row.
grep -oE '^\| [A-Za-z0-9][A-Za-z0-9-]*' THIRD_PARTY_LICENSES.md \
    | cut -c3- | grep -vx 'Package' | sort -u > "$work/table"

missing=$(comm -23 "$work/cabal" "$work/table")
extra=$(comm -13 "$work/cabal" "$work/table")

status=0
if [ -n "$missing" ]; then
    echo "Declared in volca.cabal and absent from THIRD_PARTY_LICENSES.md:"
    echo "$missing" | sed 's/^/  /'
    status=1
fi
if [ -n "$extra" ]; then
    echo "Listed in THIRD_PARTY_LICENSES.md and no longer shipped:"
    echo "$extra" | sed 's/^/  /'
    status=1
fi

if [ "$status" -ne 0 ]; then
    echo ""
    echo "The table names the libraries the binary links. Add or remove the rows above."
    exit 1
fi

echo "THIRD_PARTY_LICENSES.md matches volca.cabal."
