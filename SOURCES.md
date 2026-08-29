# Upstream Python sources

Each demo is a **thin slice** of a real GitHub Python project (or its official examples/docs).
We do not vendor whole trees — only the behaviour we port to cl-stack.

| Demo | Upstream | Slice | License | cl-stack target |
|------|----------|-------|---------|-----------------|
| `pathlib-organize` | [tfeldmann/organize](https://github.com/tfeldmann/organize) | extension-bucket file rules (pathlib walk / move) | MIT | `cl-stack-pathlib` |
| `settings-env` | [pydantic/pydantic-settings](https://github.com/pydantic/pydantic-settings) | `BaseSettings` + env prefix overlay | MIT | `cl-stack-config` |
| `requests-json` | [psf/requests](https://github.com/psf/requests) | Quickstart JSON GET/POST (`r.json()`) | Apache-2.0 | `cl-stack-http` + `json-protocol` |
| `jsonlines-io` | [wbolster/jsonlines](https://github.com/wbolster/jsonlines) | Writer/Reader one-object-per-line | BSD-3-Clause* | `serdes-protocol` |
| `object-tape` | CPython `pprint`/`ast.literal_eval` (stdlib) | REPL object dump/load stream | PSF | `io-protocol` |
| `click-naval` | [pallets/click](https://github.com/pallets/click) `examples/naval` | nested `ship`/`mine` groups | BSD-3-Clause | `cli-protocol` |
| `structlog-run` | [hynek/structlog](https://github.com/hynek/structlog) `show_off.py` | structured fields + context | Apache-2.0 / MIT | `log-protocol` |
| `flask-echo` | [pallets/flask](https://github.com/pallets/flask) tutorial shape | JSON echo + health | BSD-3-Clause | `http-server-protocol` + `cl-stack-http` |
| `websockets-echo` | [python-websockets/websockets](https://github.com/python-websockets/websockets) `example/sync/{echo,client}.py` | echo server + client round-trip | BSD-3-Clause | `ws-protocol` + `ws-backend-websocket-driver` |
| `sqlalchemy-notes` | [sqlalchemy/sqlalchemy](https://github.com/sqlalchemy/sqlalchemy) ORM tutorial | `User`/`Note` CRUD | MIT | `sql-orm` + sqlite |
| `pyjwt-hs256` | [jpadilla/pyjwt](https://github.com/jpadilla/pyjwt) README usage | encode / decode / exp | MIT | `cl-stack-jwt` |
| `authlib-pkce` | [authlib/authlib](https://github.com/authlib/authlib) OAuth2 PKCE client docs | authorize URI + S256 challenge | BSD-3-Clause | `cl-stack-oauth2` |
| `records-sql` | [kennethreitz/records](https://github.com/kennethreitz/records) | raw SQL “for humans” over SQLite | ISC | `sql-protocol` |
| `csvkit-report` | [wireservice/csvkit](https://github.com/wireservice/csvkit) | filter/aggregate CSV report | MIT | `sql-query-csv` |
| `asyncio-sleep` | [python/cpython](https://github.com/python/cpython) `asyncio` docs | `sleep` + callback | PSF | `event-protocol` + libuv |

\*Confirm in upstream `LICENSE*` if filing attribution-sensitive redistributions; demos only reimplement behaviour.

## Policy

- Python side ≈ minimal runnable adaptation of the upstream example (attribution in file header).
- Lisp side uses **only** cl-stack APIs — no workarounds. Bugs/gaps → [ISSUES.md](ISSUES.md) + upstream GH issues.
- Deep HTTP client matrix already lives in [`http-parity`](https://github.com/egao1980/http-parity); `requests-json` is a smoke port of the Quickstart only.
