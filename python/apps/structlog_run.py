"""Inspired by hynek/structlog show_off.py — structured fields."""

from __future__ import annotations

import structlog

structlog.configure(
    processors=[
        structlog.processors.add_log_level,
        structlog.processors.TimeStamper(fmt="iso"),
        structlog.processors.JSONRenderer(),
    ],
)


def main() -> None:
    log = structlog.get_logger("some_logger")
    log = log.bind(request_id="demo-1")
    log.info("informative!", some_key="some_value")
    log.warning("uh-uh!")
    log.error("omg", a_dict={"a": 42, "b": "foo"})
    log.info("boot", version="0.1.0")
    print("structlog-run emitted structured json logs")


if __name__ == "__main__":
    main()
