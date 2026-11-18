# Changelog entries

Each pull request that changes something a user would notice writes its entry
in a new file here, `<branch>.<section>.md`, the section one of `added`,
`changed`, `deprecated`, `removed`, `fixed` or `security`. The file holds one
or more list items in plain language, the way `CHANGELOG.md` reads.

`CHANGELOG.md` is written only at release time, by
`scripts/changelog.sh release X.Y.Z`, and `scripts/changelog.sh show` prints
what is waiting. Two pull requests never edit the same file, so neither has
to catch up with the other's entry.
