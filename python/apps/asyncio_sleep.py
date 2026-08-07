"""Inspired by CPython asyncio docs — sleep + callback ordering."""

from __future__ import annotations

import asyncio


async def main_async() -> None:
    order: list[str] = []
    order.append("start")
    await asyncio.sleep(0.05)
    order.append("awake")
    print(f"order={order}")
    assert order == ["start", "awake"]


def main() -> None:
    asyncio.run(main_async())


if __name__ == "__main__":
    main()
