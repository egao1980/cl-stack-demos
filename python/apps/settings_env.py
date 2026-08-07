"""Inspired by pydantic/pydantic-settings — env-prefixed settings over a TOML file."""

from __future__ import annotations

import os
import tomllib
from pathlib import Path


def load_settings(path: Path, prefix: str = "DEMO") -> dict:
    with path.open("rb") as f:
        data = tomllib.load(f)
    # env overlay: DEMO_DATABASE__HOST → database.host
    for key, value in os.environ.items():
        if not key.startswith(prefix + "_"):
            continue
        parts = key[len(prefix) + 1 :].lower().split("__")
        cur = data
        for p in parts[:-1]:
            cur = cur.setdefault(p, {})
        cur[parts[-1]] = value
    return data


def main() -> None:
    root = Path(__file__).resolve().parents[2]
    os.environ["DEMO_DATABASE__HOST"] = "db.example"
    cfg = load_settings(root / "fixtures" / "config.toml")
    print(f"app={cfg['app_name']} host={cfg['database']['host']} port={cfg['database']['port']}")
    assert cfg["database"]["host"] == "db.example"
    assert cfg["database"]["port"] == 5432


if __name__ == "__main__":
    main()
