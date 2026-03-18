"""Ingredient substitution workflow using VoLCA.

Three phases:
1. Find all "at plant" activities → export CSV
2. Find upstream "at farm" ingredients for wheat flour → export CSV with paths
3. Substitute conventional wheat with organic wheat → compare supply chains

Usage:
    python -m examples.ingredient_substitution
"""

import csv
import sys
from pathlib import Path

# Add parent to path for development (before pip install)
sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

from volca import Client, Server


def to_csv(rows: list, filename: str, fields: list[str]) -> None:
    """Write a list of objects to CSV."""
    with open(filename, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fields)
        writer.writeheader()
        for row in rows:
            writer.writerow({k: getattr(row, k) for k in fields})
    print(f"  Written {len(rows)} rows to {filename}")


def main():
    config = "volca.toml"
    db_name = "agribalyse-3.2"

    with Server(config=config) as srv:
        c = Client(base_url=srv.base_url, db=db_name, password=srv.password)

        # ── Phase 1: All "at plant" activities ──────────────────────────
        print("Phase 1: Searching for 'at plant' activities...")
        plants = c.search_activities(name="at plant", limit=10000)
        print(f"  Found {len(plants)} 'at plant' activities")
        to_csv(
            plants,
            "at_plant.csv",
            ["process_id", "name", "location", "product", "product_amount", "product_unit"],
        )

        # ── Phase 2: Wheat flour upstream "at farm" ────────────────────
        print("\nPhase 2: Searching for wheat flour type 55...")
        wheat_flour = c.search_activities(name="Wheat flour, type 55", limit=10)
        if not wheat_flour:
            print("  ERROR: 'Wheat flour, type 55' not found")
            return

        flour = wheat_flour[0]
        print(f"  Found: {flour.name} ({flour.location}) [{flour.process_id}]")

        chain = c.get_supply_chain(flour.process_id, name="at farm", limit=100)
        print(f"  Total upstream activities: {chain.total_activities}")
        print(f"  'at farm' matches: {chain.filtered_activities}")

        if chain.entries:
            to_csv(
                chain.entries,
                "wheat_flour_farm_ingredients.csv",
                ["process_id", "name", "location", "quantity", "unit", "scaling_factor"],
            )
            print("  Top 5 'at farm' ingredients:")
            for entry in chain.entries[:5]:
                path_str = " → ".join(n.name for n in entry.path) if entry.path else ""
                print(f"    {entry.quantity:.4f} {entry.unit} of {entry.name} ({entry.location})")
                if path_str:
                    print(f"      Path: {path_str}")

        # ── Phase 3: Substitute conventional → organic wheat ───────────
        print("\nPhase 3: Ingredient substitution (conventional → organic wheat)...")

        # Find conventional wheat (the "from")
        conv_query = "Soft wheat grain, conventional, breadmaking quality, 15% moisture, at farm gate"
        conv_results = c.search_activities(name=conv_query, limit=10)
        conv_fr = [a for a in conv_results if a.location == "FR"]
        if not conv_fr:
            print(f"  ERROR: conventional wheat not found (query: {conv_query!r})")
            return
        from_activity = conv_fr[0]
        print(f"  From: {from_activity.name} ({from_activity.location}) [{from_activity.process_id}]")

        # Find organic wheat (the "to")
        org_query = "Soft wheat grain, organic, 15% moisture, Central Region, at feed plant"
        org_results = c.search_activities(name=org_query, limit=10)
        org_fr = [a for a in org_results if a.location == "FR"]
        if not org_fr:
            print(f"  ERROR: organic wheat not found (query: {org_query!r})")
            return
        to_activity = org_fr[0]
        print(f"  To:   {to_activity.name} ({to_activity.location}) [{to_activity.process_id}]")

        # Create variant via Sherman-Morrison rank-1 update
        variant = c.create_variant(
            flour.process_id,
            [{"from": from_activity.process_id, "to": to_activity.process_id}],
        )
        print(f"\n  Variant created: {len(variant.substitutions)} substitution(s)")
        print(f"  Modified supply chain: {variant.total_activities} activities")

        for sub in variant.substitutions:
            print(f"    {sub.from_id} → {sub.to_id} (coeff: {sub.coefficient:.6f})")

        if variant.supply_chain:
            print(f"\n  Top 10 supply chain entries (variant):")
            for entry in variant.supply_chain[:10]:
                print(f"    {entry.quantity:.6f} {entry.unit} of {entry.name} ({entry.location})")


if __name__ == "__main__":
    main()
