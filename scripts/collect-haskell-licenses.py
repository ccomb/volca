#!/usr/bin/env python3
"""Copy the license files of every Haskell package linked into exe:volca.

Usage, from the folder of the cabal.project the engine was built with:
    python3 <path to>/scripts/collect-haskell-licenses.py <out-dir>

Reads the build plan cabal wrote (dist-newstyle/cache/plan.json), follows the
library dependencies of exe:volca, and writes one <name>-<version>/ folder per
package into <out-dir>, emptied first, with the license files the package
installed. Test, benchmark, setup and build-tool dependencies are not linked,
so they are not followed.

Fails, naming the package, when one has no license file: shipping its code
without its notice is what the licenses forbid. A package that installs none
gets its text in licenses/haskell-overrides/<name>/ of this repository.
"""

import glob
import json
import os
import shutil
import subprocess
import sys

OVERRIDES = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "licenses", "haskell-overrides")


def library_depends(unit):
    """The unit ids a unit's library links, or None for a unit that is not
    a library. A package built as a whole (a custom Setup) lists its
    components in one unit."""
    if "components" in unit:
        lib = unit["components"].get("lib")
        return None if lib is None else lib.get("depends", [])
    name = unit.get("component-name", "lib")
    return unit.get("depends", []) if name == "lib" or name.startswith("lib:") else None


def linked_units(plan):
    """Every library unit exe:volca links, its own library included."""
    units = {u["id"]: u for u in plan["install-plan"]}
    roots = [u for u in units.values() if u["pkg-name"] == "volca" and u.get("component-name") == "exe:volca"]
    if len(roots) != 1:
        sys.exit("plan.json has no exe:volca: build the engine first")
    seen, todo = set(), list(roots[0]["depends"])
    while todo:
        uid = todo.pop()
        if uid in seen:
            continue
        unit = units[uid]
        deps = library_depends(unit)
        if deps is None:
            sys.exit(f"exe:volca depends on {uid}, which is not a library")
        seen.add(uid)
        todo.extend(deps)
    return [units[uid] for uid in seen]


def run(*args):
    return subprocess.run(args, check=True, capture_output=True, text=True).stdout.strip()


def ghc_doc_folders(compiler_id):
    """The folders GHC installed its own packages' license files in, by
    package: share/doc/<abi>/<name>-<version>/ next to its libdir on Linux and
    macOS, and the same under its root on Windows."""
    ghc = shutil.which(compiler_id) or "ghc"
    if f"ghc-{run(ghc, '--numeric-version')}" != compiler_id:
        sys.exit(f"{ghc} is not the compiler of the build plan ({compiler_id})")
    libdir = run(ghc, "--print-libdir")
    roots = {os.path.normpath(os.path.join(libdir, *[".."] * up)) for up in range(4)}
    folders = {}
    for root in roots:
        for folder in glob.glob(os.path.join(root, "share", "doc", "*", "*")) + glob.glob(
            os.path.join(root, "doc", "*", "*")
        ):
            folders.setdefault(os.path.basename(folder), set()).add(folder)
    return folders


def license_files(folder):
    """The files of an installed doc folder: cabal installs a package's
    license-files there, and its haddock pages in a subfolder."""
    return sorted(os.path.join(folder, f) for f in os.listdir(folder) if os.path.isfile(os.path.join(folder, f)))


def main():
    if len(sys.argv) != 2:
        sys.exit("usage: python3 collect-haskell-licenses.py <out-dir>")
    out = sys.argv[1]
    with open(os.path.join("dist-newstyle", "cache", "plan.json"), encoding="utf-8") as f:
        plan = json.load(f)
    store = os.path.join(run("cabal", "path", "--store-dir"), "*")
    ghc_docs = ghc_doc_folders(plan["compiler-id"])

    packages = {}
    for unit in linked_units(plan):
        if unit.get("style") == "local":
            continue
        nv = f"{unit['pkg-name']}-{unit['pkg-version']}"
        if unit["type"] == "pre-existing":
            found = ghc_docs.get(nv, set())
            if unit["pkg-name"] == "rts":
                # The runtime installs no doc folder; it is GHC's, under GHC's
                # license, the one ghc-internal installs.
                found = {f for k, v in ghc_docs.items() if k.startswith("ghc-internal-") for f in v}
            if len(found) > 1:
                sys.exit(f"{nv}: more than one GHC doc folder: {sorted(found)}")
        else:
            found = set(glob.glob(os.path.join(store, unit["id"], "share", "doc")))
        packages.setdefault(nv, (unit["pkg-name"], set()))[1].update(
            f for folder in found for f in license_files(folder)
        )

    missing = []
    for nv, (name, files) in packages.items():
        override = os.path.join(OVERRIDES, name)
        if not files and os.path.isdir(override):
            files.update(license_files(override))
        if not files:
            missing.append(nv)
    if missing:
        for nv in sorted(missing):
            print(f"No license file for {nv}: add its text to licenses/haskell-overrides/<name>/", file=sys.stderr)
        sys.exit(1)

    shutil.rmtree(out, ignore_errors=True)
    for nv, (_, files) in sorted(packages.items()):
        os.makedirs(os.path.join(out, nv))
        for f in files:
            shutil.copy(f, os.path.join(out, nv, os.path.basename(f)))
    print(f"Wrote the license files of {len(packages)} Haskell packages to {out}")


if __name__ == "__main__":
    main()
