# Real-data regression run-book for PR #41 (`fix/cross-db-regional-lcia`)

## Why this exists

PR #41's test-plan item 2 reads:

> Diff regional method scores on a cross-database export against
> pre-fix output and confirm only dep-database-driven cells move.

In-tree HSpec covers the synthetic gap-and-fix proof point (regional
score = 5.0 on a two-DB fixture) and the three non-regression invariants
(non-regional methods unchanged, root-only recipes unchanged, merged
inventory = Σ per-DB matvec). What HSpec cannot cover without loading
real PEF/EF3.1 method fixtures into the suite is "no unintended movement
on real-world cells." This run-book is the manual gate.

Run it once before merging; attach the diff summary to the PR.

## What we expect

For a real cross-DB recipe (Agribalyse root + Ecoinvent dep, EF 3.1 method
set), the fix changes scores **only** on regional methods **only** on
pids that actually consume a dep-DB activity carrying a regional CF
emission. Specifically:

- Non-regional methods (e.g. GWP100, ODP) → bit-identical scores
  before/after.
- Regional methods (e.g. AcidEP, WaterUse) on pids with no dep-DB reach
  → bit-identical.
- Regional methods on pids with dep-DB reach → score moves *up* (the
  silently under-counted dep contribution is now applied). The
  magnitude of the move scales with the cross-DB link coefficient and
  the dep DB's regional CF density.

Any cell that moves outside this rule is a regression to investigate.

## Prereqs

- Two databases loaded:
  - `agribalyse` (root)
  - `ecoinvent` (dep, with `dbDependsOn` set or `dbCrossDBLinks`
    pointing at `agribalyse`)
- An EF 3.1 method collection loaded.
- `jq`, `curl`, GNU `diff` available locally.
- The volca server running on `:8080` (or adjust `BASE` below).

## Step 1 – capture the pre-fix baseline

Check out the parent of PR #41's first commit, rebuild, run the server:

```bash
git rev-parse HEAD                 # save current ref
git checkout 148d6bb               # parent of PR #41's commits (main at time of writing)
./gen-version.sh && cabal build volca:exe:volca
cabal run volca:exe:volca -- volca.toml &
```

Curate ~10 pids that span cases (some with dep reach, some without). A
quick way to find candidates: ask `/api/v1/activities` for the root DB,
then filter by those whose supply chain touches the dep DB. Save the
list to `pids.txt` (one pid per line).

Collect baseline scores:

```bash
BASE=http://localhost:8080
mkdir -p pre-fix
while read pid; do
  curl -s "$BASE/api/v1/databases/agribalyse/activities/$pid/lcia-batch?collection=EF-3.1" \
    > "pre-fix/$pid.json"
done < pids.txt
```

## Step 2 – capture the post-fix output

```bash
git checkout fix/cross-db-regional-lcia
./gen-version.sh && cabal build volca:exe:volca
# restart server …
mkdir -p post-fix
while read pid; do
  curl -s "$BASE/api/v1/databases/agribalyse/activities/$pid/lcia-batch?collection=EF-3.1" \
    > "post-fix/$pid.json"
done < pids.txt
```

## Step 3 – diff and classify

Extract `(pid, methodId, score)` triples and join:

```bash
for pid in $(cat pids.txt); do
  jq -r --arg pid "$pid" \
    '.results[] | [$pid, .methodId, .score] | @tsv' \
    "pre-fix/$pid.json"
done | sort > pre.tsv

for pid in $(cat pids.txt); do
  jq -r --arg pid "$pid" \
    '.results[] | [$pid, .methodId, .score] | @tsv' \
    "post-fix/$pid.json"
done | sort > post.tsv

# Moved cells (any score change beyond float noise):
join -t $'\t' -j 2 pre.tsv post.tsv \
  | awk -F'\t' '{ d = $4 - $5; if (d < -1e-9 || d > 1e-9) print }' \
  > moved.tsv
```

For every line in `moved.tsv`, verify:

1. The `methodId` resolves to a regional method (its `MethodTables.mtRegionalizedCF`
   is non-empty after build). Quickest check: look up the method in
   the EF 3.1 collection JSON and confirm at least one CF has a
   `consumerLocation` field.
2. The `pid` actually consumes a dep-DB activity. Run
   `/api/v1/databases/agribalyse/activities/$pid/supply-chain` and grep
   for entries with `databaseName != "agribalyse"`.
3. The score change is **positive** (the fix recovers the missing
   contribution, never zeros a previously-correct score).

Any moved cell failing one of these rules is a regression – bisect the
PR commits to find which one introduced it.

## Step 4 – record the result

Add a comment on PR #41 with:

- Number of pids tested.
- Number of moved cells.
- Number of unmoved cells (should be the bulk).
- A few sample moved cells (pid, method, pre, post, % change).
- Confirmation that the three classification rules hold for every
  moved cell.

Then check the box in the PR description.

## When to re-run

- Before any merge that touches `Service.goWithSubsAndDeps`,
  `SharedSolver.goWithDepsFromScalings`, `SharedSolver.mergeSolutions`,
  `Method.Mapping.sumRegionalizedLCIAScoreCrossDB`, or
  `Method.Mapping.computeLCIAScoreSetFromTables`.
- Before any release that bundles a method-set or DB-graph change.

The synthetic HSpec suite catches the algebra; this run-book catches
the integration with real method tables and real cross-DB topologies.
