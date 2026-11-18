#!/bin/sh
# Changelog entries wait in changelog.d/, one new file per pull request, and
# only a release writes them into CHANGELOG.md. Two pull requests never edit
# the same file, so neither has to catch up with the other's entry.
#
#   scripts/changelog.sh check          refuse a misnamed or malformed entry
#   scripts/changelog.sh show           print the entries waiting for a release
#   scripts/changelog.sh release X.Y.Z  write them into CHANGELOG.md, delete them
set -eu
cd "$(dirname "$0")/.."
export LC_ALL=C

dir=changelog.d
sections='added changed deprecated removed fixed security'

# The entry files of one section, sorted by name.
entries() {
    for f in "$dir"/*."$1".md; do
        if [ -e "$f" ]; then echo "$f"; fi
    done
}

check() {
    status=0
    for f in "$dir"/*; do
        [ -e "$f" ] || continue
        name=${f#"$dir"/}
        [ "$name" = README.md ] && continue
        section=${name%.md}
        section=${section##*.}
        stem=${name%."$section".md}
        if [ "$stem" = "$name" ] || [ -z "$stem" ] ||
            [ -n "$(printf '%s' "$name" | tr -d 'A-Za-z0-9._-')" ] ||
            ! printf ' %s ' "$sections" | grep -q " $section "; then
            echo "$f: name it $dir/<branch>.<section>.md, the section one of: $sections"
            status=1
        elif ! head -n 1 "$f" | grep -q '^- '; then
            echo "$f: an entry starts with '- ' on its first line"
            status=1
        elif grep -qvE '^(- |  |$)' "$f"; then
            echo "$f: each line opens an entry with '- ' or continues one with two spaces"
            status=1
        fi
    done
    if grep -q '^## \[Unreleased\]' CHANGELOG.md; then
        echo "CHANGELOG.md has an [Unreleased] section: write each entry in $dir/<branch>.<section>.md instead"
        status=1
    fi
    return "$status"
}

show() {
    for s in $sections; do
        files=$(entries "$s")
        [ -n "$files" ] || continue
        printf '### %s\n\n' "$(printf '%s' "$s" | awk '{ print toupper(substr($0, 1, 1)) substr($0, 2) }')"
        # Blank lines at the end of a file would split the list.
        for f in $files; do
            awk 'NF { for (; blank > 0; blank--) print ""; print; next } { blank++ }' "$f"
        done
        echo
    done
}

release() {
    version=$1
    if ! printf '%s\n' "$version" | grep -Eqx '[0-9]+\.[0-9]+\.[0-9]+'; then
        echo "usage: $0 release X.Y.Z"
        exit 1
    fi
    check
    if [ -z "$(show)" ]; then
        echo "no entry in $dir/ to release"
        exit 1
    fi
    first=$(grep -n -m 1 '^## ' CHANGELOG.md | cut -d: -f1)
    if [ -z "$first" ]; then
        echo "CHANGELOG.md has no released section to write above"
        exit 1
    fi
    tmp=$(mktemp)
    {
        head -n "$((first - 1))" CHANGELOG.md
        printf '## [%s] - %s\n\n' "$version" "$(date +%F)"
        show
        tail -n "+$first" CHANGELOG.md
    } >"$tmp"
    # Copied over rather than moved, so the file keeps its own permissions.
    cat "$tmp" >CHANGELOG.md
    rm "$tmp"
    for s in $sections; do
        for f in $(entries "$s"); do rm "$f"; done
    done
}

case "${1:-}" in
    check) check ;;
    show) show ;;
    release) release "${2:-}" ;;
    *)
        echo "usage: $0 check | show | release X.Y.Z"
        exit 1
        ;;
esac
