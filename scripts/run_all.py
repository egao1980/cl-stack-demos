#!/usr/bin/env python3
"""Run all Python reference apps."""

from __future__ import annotations

import importlib
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "python"))

APPS = [
    "pathlib_organize",
    "settings_env",
    "requests_json",
    "jsonlines_io",
    "object_tape",
    "click_naval",
    "structlog_run",
    "flask_echo",
    "websockets_echo",
    "sqlalchemy_notes",
    "pyjwt_hs256",
    "authlib_pkce",
    "records_sql",
    "csvkit_report",
    "asyncio_sleep",
]


def main() -> int:
    failed = []
    for name in APPS:
        print(f"\n======== {name} ========")
        try:
            mod = importlib.import_module(f"apps.{name}")
            mod.main()
            print(f"OK   {name}")
        except Exception as e:
            print(f"FAIL {name}: {e}")
            failed.append(name)
    print(f"\n; ok={len(APPS) - len(failed)} fail={len(failed)}")
    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
