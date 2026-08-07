# Coverage matrix

Status after `ros -l scripts/bootstrap.lisp -l scripts/run-all.lisp` (2026-08-07):

| Demo | Upstream | Status | Notes |
|------|----------|--------|-------|
| pathlib-organize | tfeldmann/organize | ok | memory FS + bytes |
| settings-env | pydantic-settings | ok | |
| requests-json | psf/requests | ok | local echo |
| jsonlines-io | wbolster/jsonlines | ok | |
| object-tape | pprint / literal_eval | ok | |
| click-naval | pallets/click | ok | |
| structlog-run | hynek/structlog | ok | |
| flask-echo | pallets/flask | ok | |
| websockets-echo | websockets | ok | |
| sqlalchemy-notes | sqlalchemy | ok | |
| pyjwt-hs256 | jpadilla/pyjwt | ok* | *`expired-p :verify t` bug — [jwt#6](https://github.com/egao1980/cl-stack-jwt/issues/6); demo uses `:verify nil` for expired case only |
| authlib-pkce | authlib | ok | |
| records-sql | kennethreitz/records | ok | |
| csvkit-report | csvkit | ok | |
| asyncio-sleep | asyncio | ok | |

Python references: `uv run python scripts/run_all.py` → **15/15 ok**.
