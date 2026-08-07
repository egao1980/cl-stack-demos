"""Inspired by jpadilla/pyjwt README usage — HS256 encode/decode/exp."""

from __future__ import annotations

import time

import jwt


def main() -> None:
    key = "secret"
    exp = int(time.time()) + 3600
    token = jwt.encode({"some": "payload", "exp": exp}, key, algorithm="HS256")
    claims = jwt.decode(token, key, algorithms=["HS256"])
    print(f"token={token}")
    print(f"claims={claims}")
    assert claims["some"] == "payload"
    old = jwt.encode({"some": "old", "exp": int(time.time()) - 10}, key, algorithm="HS256")
    try:
        jwt.decode(old, key, algorithms=["HS256"])
        raise AssertionError("expected ExpiredSignatureError")
    except jwt.ExpiredSignatureError:
        print("expired ok")


if __name__ == "__main__":
    main()
