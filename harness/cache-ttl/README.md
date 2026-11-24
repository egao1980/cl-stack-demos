# Cache demo: the in-memory TTL cache, sub-second and atomic

`harness-demos-cache-ttl` is a tour of **cache-protocol 0.2.0** from GHCR: its in-memory TTL cache, the `cachetools.TTLCache` / redis-py-shaped job behind the [Python to Common Lisp](https://egao1980.github.io/python-to-cl/) cache page. It shows the fixes for three issues that page linked:

- [cache-protocol#1](https://github.com/egao1980/cache-protocol/issues/1): a TTL lived up to a second too long (the clock was `get-universal-time`), and `cache-incr` / `cache-cas` were not atomic (lost increments, double CAS winners, a GC crash under 8 threads);
- [cache-protocol#2](https://github.com/egao1980/cache-protocol/issues/2): `cache-incr` dropped the entry's TTL, so a rate-limit window never ended;
- [cache-protocol#4](https://github.com/egao1980/cache-protocol/issues/4): `cache-incr` took `"007"`, `"-0"` and results past int64, which Redis `INCR` refuses.

Every result with a known answer is checked (48 checks), and the summary line reports the mismatches. Everything is in [`src/demo.lisp`](src/demo.lisp). [`demo-output.txt`](demo-output.txt) is the output of the last run, on macOS arm64 with SBCL 2.6.9, home and temp paths replaced by `~` and `<tmp>`.

## Run

```bash
cd harness/cache-ttl
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-cache-ttl")' \
  --eval '(harness-demos-cache-ttl:run-demos)' > demo-output.txt 2>&1
```

No server and no network step after the packages are installed. A run takes about five seconds, most of it the 1.1 s and 0.35 s sleeps of section 1 and the thread races of section 4. `run-demos` returns `NIL` when a check fails or a section stops on an error, and CI then exits 1.

## What each section shows

1. **The default clock.** `monotonic-seconds` (CLOCK_MONOTONIC on SBCL for Linux, CLOCK_MONOTONIC_RAW on SBCL for macOS, `get-internal-real-time` elsewhere) and its step `+monotonic-tick+`. Issue #1's repro: a `:ttl 1` put just after `get-universal-time` steps is gone 1.1 s later (0.1.0 still returned it). A `:ttl 0.3` entry is gone after 0.35 s. Of 200 puts with `:ttl 1/200` (5 ms), none is gone before 5 ms have elapsed, because a positive TTL counts from the end of the clock tick. The lifetime of 20 ms entries polled without sleeping (20.000 to 20.002 ms in the committed run).
2. **An injected clock is exact.** With `:clock (lambda () now)`, an entry put at t=1000 with `:ttl 600` is there at t=1599 and gone at t=1600 (cachetools: expired once `timer >= expires`); `cache-delete` of the expired key answers `NIL`, as Redis `DEL` answers 0. A `:ttl 1/2` and a float `:ttl 0.1` (rationalized to 1/10) at hundredths of a second.
3. **TTL values.** `nil`, `1/3`, `0`, `-5`, double and single `+inf` (never expires) and `-inf` (expires at once), as in cachetools, read at t=0 and t=10^9. A NaN TTL is a `cache-error`, signalled before the lock is taken.
4. **Atomicity.** 8 threads × 20000 `cache-incr` give 160000 (0.1.0: 30609 to 56113). Mixed `:delta 3` / `:delta -2` threads give the exact sum. 8 threads racing `cache-cas N → N+1` have exactly as many wins as the final value (0.1.0: 33983 wins for value 31100). 8 threads doing `cache-cas "lock" nil → "worker-i"` (create if absent) have exactly one winner.
5. **`cache-incr` keeps the TTL.** A 3-requests-per-60-s rate limiter on an injected clock: the first request creates the counter with `cache-cas` `nil → 1 :ttl 60`, the next ones `cache-incr` it. The fourth request in the window is refused (429), and at t+60 the window starts again.
6. **Redis `INCR` rules.** A table of 19 values and deltas: decimal text as Redis's `string2ll` accepts it (`"41"`, `"-1"`, `"0"`), refusals for `"007"`, `"-0"`, `"00"`, `" 1"`, `"+1"`, `"1-2"`, `""`, `"abc"`, `1.5`, `:keyword`, and the int64 edges (`9223372036854775806 + 1` is fine, `9223372036854775807 + 1` overflows, the same at the negative end, a 20-digit value is out of range). A refused `incr` leaves the value as it was.
7. **`cache-cas-mismatch`.** The condition's expected and actual values, and the `use-value` restart taken from `handler-bind`.

## Issues found

None. The demo found no bug in cache-protocol 0.2.0. The redis backend's connect work ([cache-backend-redis#3](https://github.com/egao1980/cache-backend-redis/issues/3), PR [#6](https://github.com/egao1980/cache-backend-redis/pull/6)) is still open, so this demo does not cover Redis; [`services/`](../services/) (cl-stack-demos#60) does once it is published.
