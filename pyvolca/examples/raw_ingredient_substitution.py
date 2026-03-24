"""Bulk raw-ingredient substitution across processed food products.

Finds processed ingredients in Agribalyse, identifies their upstream raw
ingredients (farm-gate activities), and substitutes them with alternatives
using Sherman-Morrison rank-1 updates.

Workflow:
1. Find processed food products (via classification or name patterns)
2. For each, discover raw ingredients in the supply chain
3. Apply substitutions from a hardcoded mapping table
4. Compare original vs variant supply chains

Usage:
    python -m examples.raw_ingredient_substitution
"""

import csv
import sys
from dataclasses import dataclass
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

from volca import Client
from volca.types import SupplyChainEntry

# ── Raw ingredient substitution table ─────────────────────────────────
# Each entry: (search query for source, location, search query for target, location)
# These are farm-gate activities to swap in supply chains.
SUBSTITUTIONS = [
    (
        "Soft wheat grain, conventional, breadmaking quality, 15% moisture, at farm gate",
        "FR",
        "Soft wheat grain, organic, 15% moisture, Central Region, at feed plant",
        "FR",
    ),
]


@dataclass
class RawIngredient:
    """A raw ingredient found in a supply chain, with its direct consumer."""
    entry: SupplyChainEntry
    consumer_id: str  # ProcessId of the activity that directly consumes this ingredient


def find_activity(client: Client, query: str, location: str | None = None):
    """Find a single activity by name query, optionally filtered by location."""
    results = client.search_activities(name=query, geo=location, limit=5)
    if not results:
        results = client.search_activities(name=query, limit=10)
        if location:
            results = [a for a in results if a.location == location]
    return results[0] if results else None


def find_raw_ingredients(client: Client, process_id: str, source_ids: set[str]) -> list[RawIngredient]:
    """Find raw ingredients from source_ids in a product's supply chain, with their consumers."""
    chain = client.get_supply_chain(process_id, name="at farm", limit=200)

    # Build edge map: from_id → list of to_ids (supplier → consumers)
    consumers_of: dict[str, list[str]] = {}
    for edge in chain.edges:
        consumers_of.setdefault(edge.from_id, []).append(edge.to_id)

    results = []
    for entry in chain.entries:
        if entry.process_id in source_ids:
            # Find the direct consumer of this raw ingredient
            consumer_ids = consumers_of.get(entry.process_id, [])
            consumer = consumer_ids[0] if consumer_ids else process_id
            results.append(RawIngredient(entry=entry, consumer_id=consumer))

    return sorted(results, key=lambda r: abs(r.entry.quantity), reverse=True)


def resolve_substitutions(client: Client):
    """Resolve the substitution table into (from_activity, to_activity) pairs."""
    pairs = []
    for src_query, src_loc, tgt_query, tgt_loc in SUBSTITUTIONS:
        src = find_activity(client, src_query, src_loc)
        tgt = find_activity(client, tgt_query, tgt_loc)
        if not src:
            print(f"  WARNING: source not found: {src_query!r} ({src_loc})")
            continue
        if not tgt:
            print(f"  WARNING: target not found: {tgt_query!r} ({tgt_loc})")
            continue
        print(f"  {src.name[:60]} → {tgt.name[:60]}")
        pairs.append((src, tgt))
    return pairs


def main():
    client = Client(base_url="http://localhost:8080", db="agribalyse-3.2")

    # ── Step 1: Find processed food products ──────────────────────────
    print("Step 1: Finding processed food products...")

    by_classification = client.search_activities(
        classification="Category",
        classification_value="Food\\Transformation",
        limit=5000,
    )
    by_recipes = client.search_activities(
        classification="Category",
        classification_value="Food\\Recipes",
        limit=5000,
    )
    by_name = client.search_activities(name="at plant", limit=5000)

    # Merge and deduplicate
    seen = set()
    processed = []
    for a in by_classification + by_recipes + by_name:
        if a.process_id not in seen:
            seen.add(a.process_id)
            processed.append(a)

    print(f"  Found {len(processed)} processed food products")
    print(f"    - by classification (Food\\Transformation): {len(by_classification)}")
    print(f"    - by classification (Food\\Recipes): {len(by_recipes)}")
    print(f"    - by name ('at plant'): {len(by_name)}")

    # ── Step 2: Resolve substitution pairs ────────────────────────────
    print("\nStep 2: Resolving substitution table...")
    sub_pairs = resolve_substitutions(client)
    if not sub_pairs:
        print("  No valid substitutions found. Exiting.")
        return

    source_ids = {src.process_id for src, _ in sub_pairs}
    source_to_target = {src.process_id: tgt.process_id for src, tgt in sub_pairs}

    # ── Step 3: Scan products for matching raw ingredients ────────────
    print(f"\nStep 3: Scanning {len(processed)} products for substitutable raw ingredients...")

    applicable = []  # (product, [RawIngredient])
    scanned = 0

    for product in processed:
        raw = find_raw_ingredients(client, product.process_id, source_ids)
        if raw:
            applicable.append((product, raw))
        scanned += 1
        if scanned % 100 == 0:
            print(f"  ... scanned {scanned}/{len(processed)}, {len(applicable)} applicable so far")

    print(f"  Scanned {scanned} products, {len(applicable)} have substitutable raw ingredients")

    if not applicable:
        print("  No products found with matching raw ingredients.")
        return

    # Export list of applicable products
    with open("substitutable_products.csv", "w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["process_id", "name", "location", "raw_ingredient",
                         "raw_quantity", "raw_unit", "consumer_id"])
        for product, raws in applicable:
            for r in raws:
                writer.writerow([product.process_id, product.name, product.location,
                                 r.entry.name, f"{r.entry.quantity:.6f}", r.entry.unit,
                                 r.consumer_id])
    print(f"  Written {len(applicable)} products to substitutable_products.csv")

    # ── Step 4: Apply substitutions on a sample ───────────────────────
    print(f"\nStep 4: Applying substitutions on first {min(5, len(applicable))} products...")

    for product, raws in applicable[:5]:
        print(f"\n  Product: {product.name[:70]}")
        for r in raws:
            print(f"    {r.entry.quantity:.4f} {r.entry.unit} {r.entry.name[:60]}")
            print(f"      consumer: {r.consumer_id[:60]}")

        substitutions = [
            {"from": r.entry.process_id, "to": source_to_target[r.entry.process_id],
             "consumer": r.consumer_id}
            for r in raws
        ]

        try:
            variant_chain = client.get_supply_chain(
                product.process_id, substitutions=substitutions, limit=10
            )
            print(f"  Substituted supply chain: {variant_chain.total_activities} activities")
        except Exception as e:
            print(f"  Substitution failed: {e}")


if __name__ == "__main__":
    main()
