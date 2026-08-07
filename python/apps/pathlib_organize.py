"""Inspired by tfeldmann/organize — extension-bucket file rules via pathlib."""

from __future__ import annotations

from collections import Counter
from pathlib import Path

BUCKETS = {
    "txt": "docs",
    "md": "docs",
    "rst": "docs",
    "png": "images",
    "jpg": "images",
    "jpeg": "images",
    "gif": "images",
    "pdf": "pdf",
    "py": "code",
    "lisp": "code",
    "js": "code",
}


def bucket_for(path: Path) -> str:
    ext = path.suffix.lstrip(".").lower()
    return BUCKETS.get(ext, "other")


def organize(inbox: Path, out: Path) -> Counter[str]:
    counts: Counter[str] = Counter()
    for src in inbox.iterdir():
        if not src.is_file():
            continue
        b = bucket_for(src)
        dest = out / b / src.name
        dest.parent.mkdir(parents=True, exist_ok=True)
        dest.write_bytes(src.read_bytes())
        counts[b] += 1
    return counts


def main() -> None:
    root = Path(__file__).resolve().parents[2]
    inbox = root / "fixtures" / "organize-inbox"
    out = root / "fixtures" / "_organize-out"
    counts = organize(inbox, out)
    print(f"buckets: {dict(counts)}")
    assert counts["docs"] >= 1 and counts["images"] >= 1
    assert (out / "docs" / "note.txt").exists()


if __name__ == "__main__":
    main()
