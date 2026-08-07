"""Inspired by psf/requests Quickstart — JSON GET/POST (local httpbin-shaped server)."""

from __future__ import annotations

import json
import threading
from http.server import BaseHTTPRequestHandler, HTTPServer

import requests


class Handler(BaseHTTPRequestHandler):
    def log_message(self, *_args) -> None:
        return

    def do_GET(self) -> None:  # noqa: N802
        body = json.dumps({"url": "/get", "args": None}).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_POST(self) -> None:  # noqa: N802
        n = int(self.headers.get("Content-Length", 0))
        raw = self.rfile.read(n)
        data = json.loads(raw or b"{}")
        body = json.dumps({"json": data, "url": "/post"}).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)


def main() -> None:
    httpd = HTTPServer(("127.0.0.1", 0), Handler)
    port = httpd.server_address[1]
    t = threading.Thread(target=httpd.serve_forever, daemon=True)
    t.start()
    base = f"http://127.0.0.1:{port}"
    g = requests.get(f"{base}/get", timeout=5)
    p = requests.post(f"{base}/post", json={"key": "value"}, timeout=5)
    print(f"GET url={g.json()['url']} POST key={p.json()['json']['key']}")
    assert g.json()["url"] == "/get"
    assert p.json()["json"]["key"] == "value"
    httpd.shutdown()


if __name__ == "__main__":
    main()
