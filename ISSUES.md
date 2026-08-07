# Bugs & gaps found by demos

Filed upstream issues (do not workaround in demo code).

| Date | App | Severity | Upstream | Link | Summary |
|------|-----|----------|----------|------|---------|
| 2026-08-07 | pyjwt-hs256 | bug | egao1980/cl-stack-jwt | [#6](https://github.com/egao1980/cl-stack-jwt/issues/6) | `expired-p` with `:verify t` signals `JWT-CLAIMS-EXPIRED` instead of returning T |
| 2026-08-07 | (bootstrap) | bug | egao1980/cl-stack | [#164](https://github.com/egao1980/cl-stack/issues/164) | OCI `cl-unicode` missing `idna-mapping` required by `cl-idna` |
| 2026-08-07 | (install) | gap | egao1980/cl-stack | [#165](https://github.com/egao1980/cl-stack/issues/165) | `ensure-system-dependencies` misses OCI `tomlet` for unpublished local SUT |

## Severity

- **bug** — documented API fails / crashes / wrong result
- **gap** — expected stack capability missing or undocumented for the Python analogue

## Process

1. Reproduce with the smallest Lisp snippet from the failing app.
2. `gh issue create -R egao1980/<repo> …`
3. Add a row here; leave the app marked `bug`/`gap` in COVERAGE.md.
4. Try a different app idea rather than patching around the failure.
