"""Inspired by pallets/flask — health + JSON echo."""

from __future__ import annotations

from flask import Flask, jsonify, request

app = Flask(__name__)


@app.get("/health")
def health():
    return "ok", 200, {"Content-Type": "text/plain"}


@app.post("/echo")
def echo():
    return jsonify(request.get_json(force=True, silent=True) or {})


def main() -> None:
    client = app.test_client()
    h = client.get("/health")
    e = client.post("/echo", json={"q": 1})
    print(f"health={h.data!r} echo.q={e.get_json()['q']}")
    assert h.data == b"ok"
    assert e.get_json()["q"] == 1


if __name__ == "__main__":
    main()
