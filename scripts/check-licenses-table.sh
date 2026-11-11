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
# stanzas. Test and benchmark dependencies never reach a user, and the two
# packages that live in this repository are covered by their own sections.
#
# The Rust crates of the solver have a table of their own, held to the same
# rule against what cargo reports, and there the license is compared as well:
# cargo knows it, where volca.cabal does not.
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
' volca.cabal | grep -vx volca | sort -u > "$work/cabal"

# The package column of the table, minus its header row. A crate is written
# between backquotes, which keeps its rows out of this list.
grep -oE '^\| [A-Za-z0-9][A-Za-z0-9-]*' THIRD_PARTY_LICENSES.md \
    | cut -c3- | grep -vx 'Package\|Crate' | sort -u > "$work/table"

# The crates whose code cargo puts in the solver's archive on one platform or
# another, each with its license. A procedural macro runs while compiling and
# leaves no code of its own, so it and what it is built from are left out, and
# so are the two crates of this repository.
cargo tree --locked --manifest-path solver/Cargo.toml -p ffi \
    --edges normal,no-proc-macro --target all --prefix none --no-dedupe --format '{p}|{l}' \
    | awk -F'|' '{ split($1, name, " "); print name[1] " | " $2 }' \
    | grep -vE '^(ffi|lu) \| ' | LC_ALL=C sort -u > "$work/cargo"

# shellcheck disable=SC2016 # the backquotes are the table's, not a command
sed -nE 's/^\| `([^`]+)` \| (.*[^ ]) *\|$/\1 | \2/p' THIRD_PARTY_LICENSES.md \
    | LC_ALL=C sort -u > "$work/crates"

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

if ! diff "$work/crates" "$work/cargo" > "$work/crates.diff"; then
    echo "The crates table of THIRD_PARTY_LICENSES.md (<) and what cargo reports for solver/ (>) differ:"
    sed -n 's/^[<>]/  &/p' "$work/crates.diff"
    status=1
fi

if [ "$status" -ne 0 ]; then
    echo ""
    echo "The tables name the libraries the binary links. Add, remove or correct the rows above."
    exit 1
fi

echo "THIRD_PARTY_LICENSES.md matches volca.cabal and solver/."
