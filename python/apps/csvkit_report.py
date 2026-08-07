"""Inspired by wireservice/csvkit — filter/aggregate CSV report."""

from __future__ import annotations

import csv
from collections import defaultdict
from pathlib import Path


def main() -> None:
    root = Path(__file__).resolve().parents[2]
    path = root / "fixtures" / "csv" / "users.csv"
    totals: dict[str, list[int]] = defaultdict(lambda: [0, 0])  # n, sum score
    with path.open(newline="") as f:
        for row in csv.DictReader(f):
            if row["active"] != "1":
                continue
            city = row["city"]
            totals[city][0] += 1
            totals[city][1] += int(row["score"])
    print("csv report:")
    for city in sorted(totals):
        n, total = totals[city]
        print(f"  :CITY {city!r}  :N {n}  :TOTAL {total}")
    assert len(totals) >= 2


if __name__ == "__main__":
    main()
