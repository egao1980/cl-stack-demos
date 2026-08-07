"""Inspired by wbolster/jsonlines — one JSON object per line."""

from __future__ import annotations

import io

import jsonlines


def main() -> None:
    buf = io.StringIO()
    with jsonlines.Writer(buf) as writer:
        writer.write({"name": "ada", "n": 1})
        writer.write({"name": "grace", "n": 2})
    buf.seek(0)
    rows = list(jsonlines.Reader(buf))
    print(f"jsonl lines={len(rows)} first={rows[0]['name']}")
    assert len(rows) == 2 and rows[0]["name"] == "ada"


if __name__ == "__main__":
    main()
