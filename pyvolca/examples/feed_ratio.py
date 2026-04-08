"""Feed ratio analysis for animal-derived food products.

Uses classification paths from the database to identify animal processes
and their feed inputs. No ML model needed — categories come from the data.

Usage:
    python -m examples.feed_ratio --url http://localhost:8080 --db agribalyse-3.2
"""

import argparse
import csv
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

from volca import Client
from volca.analysis import group_supply_chain_by_classification
from volca.types import SupplyChainEntry


def select_from_list(items: list, prompt: str, fmt=str, limit: int = 10) -> int:
    """Interactive selection. Returns chosen index."""
    for i, item in enumerate(items[:limit], 1):
        print(f"  {i}. {fmt(item)}")
    while True:
        choice = input(f"\n{prompt} [1]: ").strip() or "1"
        try:
            idx = int(choice) - 1
            if 0 <= idx < min(len(items), limit):
                return idx
        except ValueError:
            pass
        print(f"  Please enter 1–{min(len(items), limit)}")


def print_supply_chain(
    groups: dict[str, list[SupplyChainEntry]], ref_amount: float, title: str,
    filter_text: str = "",
) -> list[dict]:
    """Print supply chain grouped by classification path."""
    print(f"\n{'=' * 70}")
    print(f"Supply Chain: {title}")
    print(f"Reference product: {ref_amount:.1f} kg")
    print(f"{'=' * 70}")

    if not groups:
        print("  (no entries)")
        return []

    csv_rows = []
    # Sort groups by total quantity descending
    sorted_groups = sorted(groups.items(), key=lambda g: sum(e.quantity for e in g[1]), reverse=True)

    for path, entries in sorted_groups:
        label = path
        total_qty = sum(e.quantity for e in entries)
        per_kg = total_qty / ref_amount if ref_amount > 0 else total_qty
        print(f"\n  {label} ({len(entries)} activities, {per_kg:.3f} kg/kg)")
        for e in sorted(entries, key=lambda x: -x.quantity):
            e_per_kg = e.quantity / ref_amount if ref_amount > 0 else e.quantity
            print(f"    {e_per_kg:.4f} {e.unit}/kg  {e.name} ({e.location})")
            csv_rows.append({"category": path, "name": e.name, "location": e.location,
                             "quantity": e.quantity, "unit": e.unit,
                             "per_kg_product": round(e_per_kg, 6)})

    return csv_rows


def export_csv(rows: list[dict], filename: str) -> None:
    if not rows:
        return
    with open(filename, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=rows[0].keys())
        writer.writeheader()
        writer.writerows(rows)
    print(f"\nWritten {len(rows)} rows to {filename}")


def main():
    parser = argparse.ArgumentParser(description="Supply chain analysis")
    parser.add_argument("--url", default="http://localhost:8080", help="VoLCA server URL")
    parser.add_argument("--db", default="agribalyse-3.2", help="Database name")
    parser.add_argument("--password", default="", help="Server password")
    parser.add_argument("--prefix", default=None,
                        help="Classification filter (e.g. 'Animal feed')")
    parser.add_argument("--name", default=None, help="Product search query (non-interactive)")
    args = parser.parse_args()

    c = Client(base_url=args.url, db=args.db, password=args.password)

    # Step 1: Find a product
    query = args.name or input("Search for a product: ").strip()
    products = c.search_activities(name=query, limit=20)
    if not products:
        print(f"No activities found for '{query}'")
        return

    print(f"\nFound {len(products)} activities:")
    if args.name:
        idx = 0
    else:
        idx = select_from_list(products, "Select product", lambda p: f"{p.name} ({p.location})")
    product = products[idx]
    print(f"\nAnalyzing: {product.name}")

    # Step 2: Get supply chain grouped by classification
    print("\nFetching supply chain...")
    groups = group_supply_chain_by_classification(c, product.process_id, prefix=args.prefix)

    if not groups:
        print("No entries found (try without --prefix)")
        return

    # Step 3: Show classification tree overview
    print(f"\nClassification groups ({len(groups)} categories):")
    sorted_paths = sorted(groups.keys())
    for path in sorted_paths:
        entries = groups[path]
        print(f"  [{len(entries):3d}] {path}")

    # Step 4: Let user filter categories
    filter_text = args.prefix or ""
    if not args.prefix and not args.name:
        filter_text = input("\nFilter categories (or Enter for all): ").strip()
        if filter_text:
            lower = filter_text.lower()
            groups = {k: v for k, v in groups.items() if lower in k.lower()}

    # Step 5: Display supply chain
    # Get reference amount for normalization
    activity = c.get_activity(product.process_id)
    all_products = activity.get("piActivity", {}).get("pfaAllProducts", [])
    ref_amount = float(all_products[0]["prsProductAmount"]) if all_products else 1.0

    csv_rows = print_supply_chain(groups, ref_amount, product.name, filter_text if not args.prefix else (args.prefix or ""))
    safe_name = "".join(c if c.isalnum() or c in "._-" else "_" for c in product.name)[:50]
    export_csv(csv_rows, f"supply_chain_{safe_name}.csv")


if __name__ == "__main__":
    main()
