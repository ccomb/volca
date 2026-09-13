#!/usr/bin/env python3
"""Build chem_synonyms.csv from PubChem for CAS numbers found in VoLCA databases.

Output format (semicolon-delimited; SimaPro-CSV style):

    cas;canonical_name;synonym1;synonym2;...

The first synonym returned by PubChem becomes ``canonical_name``; the rest are
deduplicated case-insensitively and capped at ``--max-synonyms`` per CAS.
Used by the post-scoring suggester in ``Method.Mapping`` to expand tokens
before Jaccard similarity (e.g. "CO2" -> {co2, carbon, dioxide}), so a flow
named "CO2" can surface a candidate CF named "Carbon dioxide".

Source: PubChem REST API (https://pubchem.ncbi.nlm.nih.gov/rest/pug/).
PubChem data is in the public domain (US National Library of Medicine).

Run this manually whenever a new database is added; commit the regenerated
``data/chem_synonyms.csv`` separately so scoring stays reproducible offline.

Examples
--------

    # Extract CAS from XML/CSV/spold files under given roots:
    scripts/build_chem_synonyms.py \\
        --extract-cas-from /path/to/bafu /path/to/agribalyse \\
        --out data/chem_synonyms.csv

    # Or from an explicit list:
    scripts/build_chem_synonyms.py --cas-list cas.txt --out data/chem_synonyms.csv
"""

from __future__ import annotations

import argparse
import csv
import json
import re
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from collections.abc import Iterable
from pathlib import Path

PUBCHEM_BASE = "https://pubchem.ncbi.nlm.nih.gov/rest/pug"
# PubChem allows ~5 requests/second for unauthenticated clients. Be polite.
RATE_LIMIT_SECONDS = 0.21
HTTP_TIMEOUT = 20

# CAS Registry Number format: 2-7 digits, hyphen, 2 digits, hyphen, 1 check digit.
# Word boundaries protect against false positives in random text.
CAS_RE = re.compile(r"\b\d{2,7}-\d{2}-\d\b")

# Surface files most likely to contain CAS – keeps the scan fast on large trees.
SCANNED_SUFFIXES = {".xml", ".csv", ".spold", ".txt"}

# A synonym longer than this is almost always junk (full sentence, structure
# string, etc.). Capping protects token expansion from explosive sets.
MAX_SYNONYM_LENGTH = 200


def fetch_synonyms(cas: str) -> list[str] | None:
    """Return PubChem synonyms for one CAS, or None if not found."""
    url = f"{PUBCHEM_BASE}/compound/name/{urllib.parse.quote(cas)}/synonyms/JSON"
    try:
        with urllib.request.urlopen(url, timeout=HTTP_TIMEOUT) as resp:
            data = json.load(resp)
    except urllib.error.HTTPError as e:
        if e.code == 404:
            return None
        raise
    info_list = data.get("InformationList", {}).get("Information", [])
    if not info_list:
        return None
    return info_list[0].get("Synonym") or None


def collect_cas(roots: Iterable[Path]) -> set[str]:
    """Recursively scan files under each root and return the set of CAS found."""
    seen: set[str] = set()
    for root in roots:
        if not root.exists():
            print(f"warning: {root} does not exist", file=sys.stderr)
            continue
        for fp in root.rglob("*"):
            if fp.is_file() and fp.suffix.lower() in SCANNED_SUFFIXES:
                try:
                    text = fp.read_text(errors="ignore")
                except OSError:
                    continue
                seen.update(CAS_RE.findall(text))
    return seen


def dedup_synonyms(syns: list[str], cap: int) -> list[str]:
    """Case-insensitive dedup preserving order; drop too-long entries; cap length."""
    seen_lower: set[str] = set()
    out: list[str] = []
    for s in syns:
        if len(s) > MAX_SYNONYM_LENGTH:
            continue
        sl = s.lower()
        if sl in seen_lower:
            continue
        seen_lower.add(sl)
        out.append(s)
        if len(out) >= cap:
            break
    return out


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    src = ap.add_mutually_exclusive_group(required=True)
    src.add_argument("--cas-list", type=Path, help="File with one CAS per line")
    src.add_argument(
        "--extract-cas-from",
        type=Path,
        nargs="+",
        metavar="DIR",
        help="Directories to scan for CAS numbers (in .xml/.csv/.spold/.txt files)",
    )
    ap.add_argument("--out", type=Path, required=True, help="Output CSV path")
    ap.add_argument(
        "--max-synonyms",
        type=int,
        default=20,
        help="Cap synonyms per CAS (default 20)",
    )
    args = ap.parse_args()

    if args.cas_list:
        cas_set = {ln.strip() for ln in args.cas_list.read_text().splitlines() if ln.strip()}
    else:
        print(f"Scanning {len(args.extract_cas_from)} root(s) for CAS...", file=sys.stderr)
        cas_set = collect_cas(args.extract_cas_from)

    cas_sorted = sorted(cas_set)
    n = len(cas_sorted)
    print(f"Fetching synonyms for {n} CAS from PubChem...", file=sys.stderr)

    rows: list[list[str]] = []
    misses = 0
    for i, cas in enumerate(cas_sorted, 1):
        if i % 100 == 0:
            print(f"  {i}/{n} ({misses} misses so far)", file=sys.stderr)
        try:
            syns = fetch_synonyms(cas)
        except urllib.error.URLError as e:
            print(f"warning: PubChem error for {cas}: {e}", file=sys.stderr)
            syns = None
        time.sleep(RATE_LIMIT_SECONDS)
        if not syns:
            misses += 1
            continue
        unique_syns = dedup_synonyms(syns, args.max_synonyms)
        if not unique_syns:
            misses += 1
            continue
        rows.append([cas, *unique_syns])

    args.out.parent.mkdir(parents=True, exist_ok=True)
    with args.out.open("w", encoding="utf-8", newline="") as f:
        w = csv.writer(f, delimiter=";", quoting=csv.QUOTE_MINIMAL)
        w.writerow(["cas", "canonical_name", "synonyms..."])
        for row in rows:
            w.writerow(row)

    print(
        f"Wrote {len(rows)} rows to {args.out} ({misses}/{n} CAS without synonyms).",
        file=sys.stderr,
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
