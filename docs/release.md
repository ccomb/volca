# Release process

This document covers releasing the volca engine. Pyvolca has its own,
parallel flow under `pyvolca-v*` tags; see `pyvolca/CHANGELOG.md`.

## Tag namespaces

| Tag pattern | What it releases | Workflow |
|---|---|---|
| `v[0-9]*` (e.g. `v0.7.0`) | volca engine | `.github/workflows/release.yml` |
| `pyvolca-v[0-9]*` | pyvolca Python package | `.github/workflows/pyvolca-release.yml` |

Each workflow's trigger uses its specific glob, so neither can fire on
the wrong tag. Release PRs are also distinct — the engine bumps
`volca.cabal`, pyvolca bumps `pyvolca/pyproject.toml`, and the same
PR should never bump both.

## One-time setup: branch protection

Configure once via the GitHub repo settings, then leave alone:

- **Require a PR before merging to main.** Disallow direct pushes.
- **Require all 4 build jobs as required status checks**
  (`build / linux-amd64`, `build / linux-arm64`, `build / windows-amd64`,
  `build / macos-arm64`). GitHub auto-disables the Squash/Merge button
  until they're green.
- **Restrict who can push tags matching `v*` to maintainers.** Add a
  rule under "Tag protection rules" — accidental tag pushes from
  contributors otherwise trigger releases.
- **Require linear history** (recommended). Optional: require signed
  commits.

## Release procedure

### 1. Open a Release PR

Create a PR titled `Release v0.7.0` (replace with the target version).
The PR should:

- Bump `volca.cabal` `version:` from `<previous>-dev` to `0.7.0` (no
  `-dev` suffix).
- Update `CHANGELOG.md` with a `## [0.7.0]` section.
- Bump `data/VERSION` only if data changed since the last release
  (Lot 2; not yet implemented).

### 2. Wait for green CI

The 4-platform build in `build.yml` must pass. Branch protection
disables the merge button until it does.

### 3. Squash-merge

Merge the PR. The squash creates a new commit on main; the workflow
will rebuild from that commit so what users download is the post-merge
state, not the PR's pre-squash artefacts.

### 4. Cut the tag

```sh
git checkout main && git pull
git tag -s v0.7.0 -m "v0.7.0"
git push origin v0.7.0
```

`release.yml` will:

1. Verify `tag == "v$(awk '/^version:/' volca.cabal)"` — fails fast if
   the cabal bump was forgotten.
2. Re-run the 4-platform build in **release mode** (`_build-matrix.yml`
   with `release: true`), packaging each platform's binary as
   `volca-<version>-<os>-<arch>.{tar.gz,zip}`.
3. Generate `SHA256SUMS` for all artefacts.
4. Create the GH Release attaching everything plus auto-generated
   release notes (PR titles since the last tag).

### 5. Bump to the next dev version

Open a small follow-up PR:

```diff
-version:             0.7.0
+version:             0.7.1-dev
```

Pick the next patch (e.g. `0.7.1-dev`) or next minor (`0.8.0-dev`)
depending on what's planned. The point is that `main` is never sitting
on a released version — the `-dev` suffix marks "in flight".

## Why explicit tags, not auto-release-on-merge?

Tags are durable handles for what shipped. Tagging is a deliberate,
signed action by a maintainer; an auto-release would couple the
"merge a feature" decision to the "ship it now" decision, making it
harder to land work without immediately releasing it.

The trade-off is one extra command (`git tag … && git push`). Worth it.
