"""Inspired by kennethreitz/records — raw SQL for humans over SQLite."""

from __future__ import annotations

import records


def main() -> None:
    db = records.Database("sqlite:///:memory:")
    db.query(
        """CREATE TABLE active_users (
             username TEXT, active INTEGER, name TEXT, user_email TEXT)"""
    )
    db.query(
        "INSERT INTO active_users VALUES (:u, :a, :n, :e)",
        u="model-t",
        a=1,
        n="Henry Ford",
        e="model-t@gmail.com",
    )
    db.query(
        "INSERT INTO active_users VALUES (:u, :a, :n, :e)",
        u="ada",
        a=1,
        n="Ada Lovelace",
        e="ada@x",
    )
    rows = db.query("SELECT * FROM active_users WHERE active = :a", a=1).all()
    print(f"rows={len(rows)} first.name={rows[0].name}")
    assert len(rows) == 2 and rows[0].name == "Henry Ford"


if __name__ == "__main__":
    main()
