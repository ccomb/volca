"""Bulk raw-ingredient substitution across processed food products.

Finds processed ingredients in Agribalyse, identifies their upstream raw
ingredients (farm-gate activities), and substitutes them with alternatives
using Sherman-Morrison rank-1 updates.

Workflow:
1. Resolve substitution table (source → target raw ingredients)
2. Use reverse supply chain (consumers endpoint) to find all products affected
3. For each affected product, find the consumer link and apply substitution
4. Compare original vs substituted supply chains

Usage:
    python -m examples.raw_ingredient_substitution
    python -m examples.raw_ingredient_substitution --url http://localhost:8080 --db agribalyse-3.2
"""

import argparse
import csv
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

from volca import Client

# ── Raw ingredient substitution table ─────────────────────────────────
# Each entry: (search query for source, location, search query for target, location)
SUBSTITUTIONS = [
    (
        "Soft wheat grain, conventional, breadmaking quality, 15% moisture, at farm gate",
        "FR",
        "Soft wheat grain, organic, 15% moisture, Central Region, at feed plant",
        "FR",
    ),
]


def find_activity(client: Client, query: str, location: str | None = None):
    """Find a single activity by name query, optionally filtered by location."""
    results = client.search_activities(name=query, geo=location, limit=5)
    if not results:
        results = client.search_activities(name=query, limit=10)
        if location:
            results = [a for a in results if a.location == location]
    return results[0] if results else None


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


def find_consumer_id(client: Client, product_id: str, supplier_id: str) -> str | None:
    """Find the direct consumer of a supplier in a product's supply chain edges."""
    chain = client.get_supply_chain(product_id, include_edges=True)
    for edge in chain.edges:
        if edge.from_id == supplier_id:
            return edge.to_id
    return None


def main():
    parser = argparse.ArgumentParser(description="Bulk raw-ingredient substitution")
    parser.add_argument("--url", default="http://localhost:8080", help="VoLCA server URL")
    parser.add_argument("--db", default="agribalyse-3.2", help="Database name")
    parser.add_argument("--password", default="", help="Server password")
    args = parser.parse_args()

    client = Client(base_url=args.url, db=args.db, password=args.password)

    # ── Step 1: Resolve substitution pairs ────────────────────────────
    print("Step 1: Resolving substitution table...")
    sub_pairs = resolve_substitutions(client)
    if not sub_pairs:
        print("  No valid substitutions found. Exiting.")
        return

    # ── Step 2: Find all products affected (reverse supply chain) ─────
    print("\nStep 2: Finding affected products via reverse supply chain...")
    applicable = []  # (consumer_activity, source, target)

    for src, tgt in sub_pairs:
        resp = client.get_consumers(src.process_id, limit=10000)
        print(f"  {src.name[:50]}: {len(resp.consumers)} downstream consumers")
        for consumer in resp.consumers:
            applicable.append((consumer, src, tgt))

    print(f"  Total: {len(applicable)} product-substitution pairs")

    if not applicable:
        print("  No affected products found.")
        return

    # Export list of applicable products
    with open("substitutable_products.csv", "w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["process_id", "name", "location", "raw_ingredient"])
        for product, src, _tgt in applicable:
            writer.writerow([product.process_id, product.name, product.location, src.name])
    print(f"  Written {len(applicable)} products to substitutable_products.csv")

    # ── Step 3: Apply substitutions on a sample ───────────────────────
    print(f"\nStep 3: Applying substitutions on first {min(5, len(applicable))} products...")

    for product, src, tgt in applicable[:5]:
        print(f"\n  Product: {product.name[:70]}")

        # Find the consumer link for this specific product
        consumer_id = find_consumer_id(client, product.process_id, src.process_id)
        if not consumer_id:
            print(f"    Could not find consumer link, skipping")
            continue

        substitutions = [{"from": src.process_id, "to": tgt.process_id, "consumer": consumer_id}]

        try:
            variant_chain = client.get_supply_chain(
                product.process_id, substitutions=substitutions, limit=10
            )
            print(f"    Substituted supply chain: {variant_chain.total_activities} activities")
        except Exception as e:
            print(f"    Substitution failed: {e}")


if __name__ == "__main__":
    main()
