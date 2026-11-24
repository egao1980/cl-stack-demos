# cl-repo client demos: 0.27.35 against 0.27.34, 0.27.38 against 0.27.37, 0.27.39 against 0.27.38

`harness-demos-cl-repo` runs the published cl-repo client the way a user runs it, before and after the fixes of [cl-repository#174](https://github.com/egao1980/cl-repository/pull/174). Release 0.27.35 has the fixes; 0.27.34 is the release before. Each command runs a client's own launcher (`roswell/cl-repo`) from its OCI-published tree, in a fresh `HOME` and against GHCR. The fresh `HOME` holds no install tree, no lock and no cl-repo configuration. Roswell's home is kept (`ROSWELL_HOME`), so Roswell's SBCL runs the clients.

The results of 0.27.35 are checked. The 0.27.34 runs are printed only, as the "before" picture. [`demo-output.txt`](demo-output.txt) is the output of the last run, on macOS arm64 (Roswell SBCL 2.4.11 for the clients, SBCL 2.6.9 for the demo image). Home and temp paths are replaced by `~` and `<tmp>`, and the client trees by `<clients>`. Compiler and download lines are left out of the listings. Everything is in [`src/demo.lisp`](src/demo.lisp).

## Run

```bash
cd harness/cl-repo
./scripts/setup.sh        # the 0.27.34 and 0.27.35 clients into .clients/ (git-ignored)
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-cl-repo")' \
  --eval '(harness-demos-cl-repo:run-demos)' > demo-output.txt 2>&1
```

`scripts/setup.sh` installs each client with that release's own `setup-client.sh`, from its image `ghcr.io/egao1980/cl-repository/cl-repository-client:<version>`. It does not touch `~/.cl-repository-client` or `~/.local/bin/cl-repo`. It needs `oras`, `python3` and `curl`. Without the 0.27.34 tree, the "before" runs are skipped. `HARNESS_CL_REPO_CLIENTS` points at another directory of client trees. The demo needs network access to GHCR and takes several minutes, most of it in section 6. It is not in the `harness-demos` CI matrix. `run-demos` returns `NIL` when a check fails or a section stops on an error.

## What each section shows

1. **#166, `cl-repo ros` without Quicklisp.** `cl-repo ros -e … -q` prints whether the image has the `QL` package. With 0.27.34 it does: Roswell's default init loads Roswell's Quicklisp, so a system the project does not declare can load from there silently. 0.27.35 starts Roswell with `+Q`. A `-Q` from the user comes later on the command line, and Roswell's later option wins, so it turns Quicklisp back on.
2. **#163, a lock entry pinned by digest.** cl-utilities is published with a commit tag only ([cl-stack-systems#80](https://github.com/egao1980/cl-stack-systems/issues/80)), so `cl-repo install cl-utilities` installs it as `latest`. A project that depends on it is synced, and its `cl-repo.lock` is shown. Then a fresh machine (a new `HOME` with the project and the lock) runs `cl-repo sync --locked`. 0.27.34 writes `:index-digest ""` and `:registry ""`, and the fresh machine dies on `Not found …/manifests/latest`. 0.27.35 records the digest and the registry, and the fresh machine pulls cl-utilities by that digest.
3. **#165, a missing minimum.** The project needs csv-protocol 0.9.0 or later, and GHCR has older versions only. 0.27.34 installs csv-protocol's dependencies and then says `system csv-protocol is not in the registry`. 0.27.35 fails with `csv-protocol 0.9.0 or later is required; the registry has … (published versions: …)` and installs nothing.
4. **#167, the newest published version.** schema-protocol 0.2.0 is installed first, and then a project that depends on schema-protocol is synced with no lock. 0.27.34 locks the installed 0.2.0. 0.27.35 sees that a newer version is published, installs it and locks it, so two machines agree on one `.asd`.
5. **#170, a qlfile-only project.** The project is the single qlfile line `ql schema-protocol-json`, with no `.asd`. `cl-repo ros` loads schema-protocol-json. 0.27.34 installs the entry alone, so the load stops on `Component "schema-protocol" not found`. 0.27.35 installs and locks the entry's closure (closer-mop, schema-protocol, schema-protocol-json), and the load succeeds.
6. **#162, six images and one fasl root.** The project depends on dexador, which comes from the client's own tree because the client uses it too. After one sync, each round empties the fasl root (`$XDG_CACHE_HOME/common-lisp/`) and starts six `cl-repo ros` runs at once. Each run has two images: its client process compiles the client, dexador included, into that root, and then its project image loads dexador from it. A round counts the runs that printed `LOADED`, and the `; compiling file "…"` lines (SBCL's `*compile-verbose*` output) of all six runs, against the number of distinct files. 0.27.34 has no lock, so images lose loads to each other: UIOP's `compile-file*` deletes the fasl, renames its staging file over it and then takes its `truename`, and another image's delete lands in between. Images also compile files that another image is compiling at the same moment. In 0.27.35, each `compile-op` holds an exclusive `flock` on `<fasl>.lock` and each `load-op` holds a shared one. A compile that finds its fasl up to date once it holds the lock is skipped. The debugger releases the locks, so an image that sits in the debugger blocks no one. Expected result: every run loads, and each file is compiled once.

Not shown: #169. The fix removes a misleading "stays loaded from ~/.cl-repository-client" warning from the client's own sync. It can be seen only when a package in the client's tree is older than the newest published one, and the 0.27.35 tree is current. The cl-repository tests cover it (`issue-169-…` and the pin test). #168 needed no client change (cl-repository#109 fixed it). The stale labels on GHCR are tracked in [cl-repository#183](https://github.com/egao1980/cl-repository/issues/183).

## Issues found

- [cl-repository#179](https://github.com/egao1980/cl-repository/issues/179): after #167, a sync over an all-installed closure lists every package's tags, so with no registry reachable it waits minutes before it keeps the installed versions. It ships in 0.27.35 and is fixed in 0.27.38: see [Offline sync and `--version`](#offline-sync-and---version-02738-against-02737) below.
- [cl-repository#182](https://github.com/egao1980/cl-repository/issues/182): with #167, a project image can recompile a client-tree fasl against a newer dependency from the install tree. It is open.
- [cl-repository#172](https://github.com/egao1980/cl-repository/issues/172) and [#173](https://github.com/egao1980/cl-repository/issues/173) are the remaining follow-ups: client-tree systems are not locked, and the fasl lock covers only SBCL off Windows.

## Offline sync and `--version`: 0.27.38 against 0.27.37

`run-offline-demos` ([`src/offline.lisp`](src/offline.lisp)) compares two published clients with the registry unreachable. 0.27.37 is the release before the fix of [cl-repository#179](https://github.com/egao1980/cl-repository/issues/179) and [#189](https://github.com/egao1980/cl-repository/issues/189). 0.27.38 has the fix ([cl-repository#191](https://github.com/egao1980/cl-repository/pull/191)). Both are the OCI-published clients, installed by `scripts/setup.sh 0.27.37 0.27.38` into `.clients/`, not clients built from a checkout. Each command runs a client's own launcher in a fresh `HOME`, as in the sections above, so every dependency of dexador comes from the client's own tree.

Offline means one of three things, and none of them touches the machine's network:
- `HTTPS_PROXY=http://127.0.0.1:9`, a loopback port where nothing listens, so every connection is refused;
- `--registry http://127.0.0.1:9`, a dead registry configured before GHCR;
- `HTTPS_PROXY=http://192.0.2.1:9`, a TEST-NET-1 address that answers nothing.

`CL_REPO_TRACE_HTTP` counts the requests that went out. A run is stopped after 600 s; the 0.27.37 black-hole run is stopped after 180 s. The 0.27.38 results are checked. [`offline-output.txt`](offline-output.txt) is the last run, on macOS arm64. The demo was driven by `cl-repo run` with the published 0.27.38 from `.clients/` in a fresh `HOME`, and the driver's own install and compile lines are left out. It took 19 minutes, nearly all of it in the 0.27.37 runs. All 19 checks held.

```bash
cd harness/cl-repo
./scripts/setup.sh 0.27.37 0.27.38
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-cl-repo")' \
  --eval '(harness-demos-cl-repo:run-offline-demos)' > offline-output.txt 2>&1
```

| Section | 0.27.37 | 0.27.38 |
|---|---|---|
| `cl-repo --version` | `Unknown command: --version`, the usage, exit 1 | `cl-repo 0.27.38 (<clients>/0.27.38/cl-oci-0.27.38/)` |
| offline (refused), `demo179a` on dexador, `sync --no-lock` | 218.9 s, 154 requests | 1.2 s, 8 requests |
| dead `--registry` beside GHCR | 424.1 s, 360 requests, 30 log lines for the dead registry | 2.6 s, 94 requests, 1 log line |
| black-holed proxy | stopped after 180 s, still waiting | 11.3 s: one 10 s connect timeout |
| offline, parse-number not installed (no lock) | 240.1 s, then `could not look up or pull parse-number` | 1.2 s, the same error, which says `Registry https://ghcr.io is unreachable` |
| offline, current `cl-repo.lock`, all installed | 1.0 s, 0 requests | 1.0 s, 0 requests |
| offline, current lock, parse-number not installed | 8.1 s, `Network failure: … failed after 4 attempts` | 1.1 s, `parse-number 1.8 (cl-repo.lock) is not installed, and https://ghcr.io could not be asked for it` |

The 8 offline requests of 0.27.38 are the first wave of the tag-list batch (8 in flight), sent before the first failure comes back. The other 22 packages' requests are not sent: a registry that has not answered gets one attempt, and once that fails, requests to it fail at once for 60 s.

## `V=$(cl-repo --version)` on an empty fasl root: 0.27.39 against 0.27.38

`run-stdout-demos` ([`src/stdout.lisp`](src/stdout.lisp)) runs what a script runs, `V=$(cl-repo --version)` in `/bin/sh`, with two published clients. 0.27.38 is the release before the fixes of [cl-repository#194](https://github.com/egao1980/cl-repository/issues/194) and [#193](https://github.com/egao1980/cl-repository/issues/193). 0.27.39 has them ([cl-repository#195](https://github.com/egao1980/cl-repository/pull/195)). Both are installed by `scripts/setup.sh 0.27.38 0.27.39` into `.clients/`. Each run gets a fresh `HOME`, and the shell sends the client's stderr to a file. An empty fasl root (`XDG_CACHE_HOME`) makes the client compile itself and its dependencies, as on the first run after an upgrade or on a CI runner without a fasl cache. Then the client runs again over the same root.

The 0.27.39 results are checked. [`stdout-output.txt`](stdout-output.txt) is the last run, on macOS arm64. The demo was driven by `cl-repo run` with the published 0.27.39 from `.clients/` in a fresh `HOME`, and the driver's own install and compile lines are left out. All 9 checks held, in 86 s.

```bash
cd harness/cl-repo
./scripts/setup.sh 0.27.38 0.27.39
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-cl-repo")' \
  --eval '(harness-demos-cl-repo:run-stdout-demos)' > stdout-output.txt 2>&1
```

| `V=$(cl-repo --version)` | 0.27.38 | 0.27.39 |
|---|---|---|
| empty fasl root | V is the version line. stderr gets 43,944 lines: 476 `; compiling file`, 3 cl-unicode `;;; Computing Hangul` notes, 2 lines of the #193 warning ("overwrite the type predicate with the slot accessor"). 28.1 s | V is the version line. stderr gets 1 line, trivial-garbage's `.asd` warning. 27.5 s |
| next run, same fasl root | V is the version line. stderr gets 2 lines: trivial-garbage and the Hangul note | V is the version line. stderr gets 1 line, trivial-garbage |
| empty fasl root, `CL_REPO_VERBOSE=1` | (no such variable) | V is the version line. stderr gets the compile, 43,739 lines and 472 `; compiling file`, with no type-predicate warning |

V was already clean in 0.27.38. Its launcher has sent the client's load output to stderr since 0.27.0, and the transcript in #194 came from a tool that shows stdout and stderr together. What 0.27.39 changes is stderr. The client's own build is kept back unless `CL_REPO_VERBOSE=1`, and a failed build shows it before the error. Two kinds of warning still go to stderr: cl-repo's own, and one a file signals while it loads. trivial-garbage's line is the second kind: ASDF warns that `trivial-garbage.asd` also defines `trivial-garbage-tests`.
