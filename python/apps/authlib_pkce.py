"""Inspired by authlib OAuth2 PKCE client — authorize URI + S256 challenge."""

from __future__ import annotations

import base64
import hashlib
import secrets
from urllib.parse import parse_qs, urlparse

from authlib.common.security import generate_token
from authlib.integrations.requests_client import OAuth2Session


def main() -> None:
    # Minimal PKCE S256 (same shape Authlib uses for code_challenge)
    verifier = generate_token(48)
    digest = hashlib.sha256(verifier.encode("ascii")).digest()
    challenge = base64.urlsafe_b64encode(digest).rstrip(b"=").decode("ascii")

    session = OAuth2Session(
        client_id="cid",
        redirect_uri="https://app/cb",
        scope="openid profile",
        code_challenge_method="S256",
    )
    uri, state = session.create_authorization_url(
        "https://as.example/authorize",
        code_challenge=challenge,
        code_challenge_method="S256",
    )
    print(f"method=S256 challenge-len={len(challenge)}")
    print(f"authorize-uri={uri}")
    assert "code_challenge" in uri and "client_id" in uri
    qs = parse_qs(urlparse(uri).query)
    assert qs["code_challenge"][0] == challenge
    assert state
    assert secrets.token_urlsafe(8)  # entropy smoke


if __name__ == "__main__":
    main()
