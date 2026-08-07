# Bugs & gaps found by demos

Filed upstream issues (do not workaround in demo code).

| Date | App | Severity | Upstream | Link | Summary |
|------|-----|----------|----------|------|---------|
| — | — | — | — | — | (none yet) |

## Severity

- **bug** — documented API fails / crashes / wrong result
- **gap** — expected stack capability missing or undocumented for the Python analogue

## Process

1. Reproduce with the smallest Lisp snippet from the failing app.
2. `gh issue create -R egao1980/<repo> …`
3. Add a row here; leave the app marked `bug`/`gap` in COVERAGE.md.
4. Try a different app idea rather than patching around the failure.
