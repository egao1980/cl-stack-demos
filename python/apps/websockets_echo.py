"""Inspired by python-websockets/websockets example/sync/{echo,client}.py."""

from __future__ import annotations

import asyncio
import threading

from websockets.sync.client import connect
from websockets.sync.server import serve


def echo(websocket) -> None:
    for message in websocket:
        websocket.send(message)


def main() -> None:
    payload = "ws-demo-py"
    got: list[str] = []

    def run_server() -> None:
        with serve(echo, "127.0.0.1", 18765) as server:
            server.serve_forever()

    t = threading.Thread(target=run_server, daemon=True)
    t.start()
    import time

    time.sleep(0.2)

    with connect("ws://127.0.0.1:18765") as websocket:
        websocket.send(payload)
        got.append(websocket.recv())
    print(f"got {got[0]!r}")
    assert got[0] == payload


if __name__ == "__main__":
    main()
