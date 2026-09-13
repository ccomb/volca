# Release process

This document covers releasing the volca engine. Pyvolca has its own,
parallel flow under `pyvolca-v*` tags; see `pyvolca/CHANGELOG.md`.

## Tag namespaces

| Tag pattern | What it releases | Workflow |
|---|---|---|
| `v[0-9]*` (e.g. `v0.7.0`) | volca engine | `.github/workflows/release.yml` |
| `pyvolca-v[0-9]*` | pyvolca Python package | `.github/workflows/pyvolca-release.yml` |

Each workflow's trigger uses its specific glob, so neither can fire on
the wrong tag. Release PRs are also distinct – the engine bumps
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
  rule under "Tag protection rules" – accidental tag pushes from
  contributors otherwise trigger releases.
- **Require linear history** (recommended). Optional: require signed
  commits.

## Release procedure

### 1. Open a Release PR

Create a PR titled `Release v0.7.0` (replace with the target version).
The PR should:

- Bump `volca.cabal` `version:` from `<previous>-dev` to `0.7.0` (no
  `-dev` suffix). Before dropping the suffix, hold the number against
  the rule in step 5: a removal or redefinition that landed since the
  last bump raises it here, `0.7.1-dev` becoming `0.8.0`.
- Update `CHANGELOG.md` with a `## [0.7.0]` section.
- Nothing to do for `data/VERSION`: it moved in the PR that changed
  `data/`, the way the wire revision moves in the PR that changes the
  wire. `scripts/check-data-version.sh` runs on every PR and again in
  `release.yml` before anything is built, and refuses both a `data/`
  that changed since the previous release under the same number and a
  number that moved with nothing behind it.

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

1. Verify `tag == "v$(awk '/^version:/' volca.cabal)"` – fails fast if
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

This is where the *next* release number is decided: step 1 above reads
the target straight from `volca.cabal`, so nothing infers it later. The
number warns users of one thing: whether a script that works today
survives the coming release.

- Something a working client relies on goes away or changes meaning: a
  field, a route or an MCP tool removed, a unit or an identifier
  redefined, in short anything an older client cannot survive (what
  makes pyvolca raise its `REQUIRED_WIRE`). **Minor**, `0.8.0-dev`. From `1.0.0` on the same rule
  reads **major**, `2.0.0-dev`.
- Everything else, however large: **patch**, `0.7.1-dev`. New routes,
  tools, formats, configuration keys and fields are additive, and so is
  a wire revision: `currentWireVersion` counts what the engine learnt to
  say, not what it stopped saying. From `1.0.0` on, additions read
  **minor** and fixes **patch**.

A removal is never a surprise: the release that introduces a replacement
keeps the old shape, and its CHANGELOG entry names the release that drops
it, the next minor (major from `1.0.0` on) at the earliest. A
redefinition has no old shape to keep, which is why the version number
is what announces it. Not knowing yet counts as nothing removed, so pick the patch; if a removal
lands after all, the release PR raises the version at the same time it
drops the `-dev` suffix. The point is that `main` is never sitting on a
released version, the `-dev` suffix marks "in flight".

A change under `data/` never touches the engine's number. Reference data
has its own, `data/VERSION`, and a score that moves on unchanged inputs
is read there: `/api/v1/version` reports it as `dataVersion`, so two
engines of one version giving two answers differ on that field.

## One-liner installers

After a `v*` release lands, end users can install with one line.
Both installers fetch the platform tarball + data bundle from the GH
Release, verify SHA256, and drop a shim that sets `VOLCA_DATA_DIR`.

**Linux + macOS (bash):**

```sh
curl -fsSL https://raw.githubusercontent.com/ccomb/volca/main/install.sh | sh
```

Pin a specific version:

```sh
curl -fsSL https://raw.githubusercontent.com/ccomb/volca/main/install.sh | sh -s -- v0.7.0
```

**Windows (PowerShell):**

```powershell
iwr -useb https://raw.githubusercontent.com/ccomb/volca/main/install.ps1 | iex
```

Both write to `~/.local/share/volca/` (Unix) or `%LOCALAPPDATA%\volca\`
(Windows) and add a shim at `~/.local/bin/volca` / `%LOCALAPPDATA%\volca\bin\volca.cmd`.

## Why explicit tags, not auto-release-on-merge?

Tags are durable handles for what shipped. Tagging is a deliberate,
signed action by a maintainer; an auto-release would couple the
"merge a feature" decision to the "ship it now" decision, making it
harder to land work without immediately releasing it.

The trade-off is one extra command (`git tag … && git push`). Worth it.
