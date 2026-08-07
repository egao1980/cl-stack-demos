# cl-stack-demos

Port **thin slices of real GitHub Python projects** onto [cl-stack](https://github.com/egao1980/cl-stack) to (a) showcase the stack, (b) find bugs, (c) find gaps.

Provenance: **[SOURCES.md](SOURCES.md)**. Failures: **[ISSUES.md](ISSUES.md)** — report upstream, do not workaround.

## Apps (15)

| Demo | Python upstream | CL packages |
|------|-----------------|-------------|
| `pathlib-organize` | tfeldmann/organize | cl-stack-pathlib |
| `settings-env` | pydantic/pydantic-settings | cl-stack-config |
| `requests-json` | psf/requests quickstart | cl-stack-http, json-protocol |
| `jsonlines-io` | wbolster/jsonlines | serdes-protocol |
| `object-tape` | pprint / literal_eval | io-protocol |
| `click-naval` | pallets/click naval | cli-protocol |
| `structlog-run` | hynek/structlog | log-protocol |
| `flask-echo` | pallets/flask | http-server-protocol, cl-stack-http |
| `websockets-echo` | python-websockets/websockets | ws-protocol |
| `sqlalchemy-notes` | sqlalchemy ORM tutorial | sql-orm |
| `pyjwt-hs256` | jpadilla/pyjwt | cl-stack-jwt |
| `authlib-pkce` | authlib PKCE | cl-stack-oauth2 |
| `records-sql` | kennethreitz/records | sql-protocol |
| `csvkit-report` | wireservice/csvkit | sql-query-csv |
| `asyncio-sleep` | asyncio docs | event-protocol |

## Run Lisp

```bash
# one-time: OCI client + tomlet pin (see cl-stack QUICKSTART)
oras pull ghcr.io/egao1980/cl-repository/cl-repository-client:0.13.0 -o /tmp/cl-repo-pull
# or resolve :latest anchor → semver (see cl-stack QUICKSTART)
# extract to ~/.local/share/cl-repository-client/ …
ros -l scripts/bootstrap.lisp -l scripts/load-oci-pins.lisp
ros -l scripts/ql-deps.lisp

# workspace siblings on ASDF path (needs egao1980/cl-unicode checkout for idna-mapping):
ros -l scripts/bootstrap.lisp -e '(asdf:load-system "websocket-driver") (asdf:load-system "clack")' \
    -l scripts/run-all.lisp
ros -l scripts/bootstrap.lisp -l scripts/run-app.lisp click-naval
```

## Run Python references

```bash
uv sync
uv run python scripts/run_all.py
uv run python -m apps.click_naval ship new Destiny
```

## Policy

No workarounds. If a CL port hits a bug/gap, file `egao1980/<pkg>` or `egao1980/cl-stack`, log it in ISSUES.md, move to another app.

## License

MIT (demo code). Upstream projects retain their licenses — see SOURCES.md.
