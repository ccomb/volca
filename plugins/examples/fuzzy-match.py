#!/usr/bin/env python3
"""Example fuzzy mapper plugin for VoLCA.

Demonstrates the JSON stdin/stdout protocol for mapper plugins.
Reads a JSON request on stdin, returns a JSON response on stdout.

Protocol:
  Input:  {"action": "match", "query": {"name": "...", "cas": "...", "compartment": "..."}}
  Output: {"target_id": "uuid", "strategy": "fuzzy", "confidence": 0.75}
       or {"matched": false}

Install: pip install thefuzz (optional, falls back to simple substring matching)

Configure in volca.toml:
  [[plugin]]
  name = "fuzzy-mapper"
  type = "mapper"
  path = "plugins/examples/fuzzy-match.py"
  priority = 25  # between name (20) and synonym (30)
"""

import json
import sys

try:
    from thefuzz import fuzz
    HAS_FUZZY = True
except ImportError:
    HAS_FUZZY = False


def simple_ratio(a: str, b: str) -> float:
    """Fallback similarity ratio when thefuzz is not installed."""
    a, b = a.lower(), b.lower()
    if a == b:
        return 1.0
    if a in b or b in a:
        return 0.8
    common = sum(1 for c in a if c in b)
    return common / max(len(a), len(b)) if max(len(a), len(b)) > 0 else 0.0


def match(query: dict) -> dict:
    """Attempt a fuzzy match. Returns match result or no-match."""
    name = query.get("name", "")
    if not name:
        return {"matched": False}

    # In a real plugin, you'd load a reference database of flow names here.
    # This example just demonstrates the protocol.
    reference_flows = {
        "carbon dioxide": "38a622c6-f086-4763-a952-7c6b3b1c42ba",
        "methane": "b53d3744-3629-4219-be20-980865e54031",
        "dinitrogen monoxide": "20f5922a-1b8a-4269-a15d-2f1a8f9a9e20",
    }

    best_score = 0.0
    best_id = None
    for ref_name, ref_id in reference_flows.items():
        if HAS_FUZZY:
            score = fuzz.ratio(name.lower(), ref_name) / 100.0
        else:
            score = simple_ratio(name, ref_name)
        if score > best_score:
            best_score = score
            best_id = ref_id

    if best_score >= 0.7 and best_id:
        return {
            "target_id": best_id,
            "strategy": "fuzzy",
            "confidence": round(best_score, 3),
        }

    return {"matched": False}


def main():
    request = json.loads(sys.stdin.read())
    action = request.get("action", "match")

    if action == "match":
        result = match(request.get("query", {}))
    else:
        result = {"error": f"Unknown action: {action}"}

    json.dump(result, sys.stdout)
    sys.stdout.write("\n")


if __name__ == "__main__":
    main()
