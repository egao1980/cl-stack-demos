"""Inspired by CPython pprint + ast.literal_eval — one printed object per line."""

from __future__ import annotations

import ast
import io
import pprint


def main() -> None:
    objects: list[object] = [{"user": "ada", "id": 1}, "hello", 42]
    buf = io.StringIO()
    for o in objects:
        buf.write(pprint.pformat(o))
        buf.write("\n")
    got = [ast.literal_eval(line) for line in buf.getvalue().splitlines() if line.strip()]
    print(f"tape {got}")
    assert got == objects


if __name__ == "__main__":
    main()
