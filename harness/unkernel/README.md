# Unkernel demo: commands confined in Linux-in-wasm

`harness-demos-unkernel` uses **compute-backend-unkernel 0.1.0** (R4h-4) from GHCR. This is compute-protocol's portable fallback sandbox. Each session is one [unkernel](https://www.npmjs.com/package/unkernel) guest (Linux compiled to WebAssembly, Alpine userland) inside a Node host process. It needs no hypervisor and no root, so it runs on macOS as well as Linux. Everything is in [`src/demo.lisp`](src/demo.lisp), and [`demo-output.txt`](demo-output.txt) is the output of the last run, on macOS arm64 with Node 26. Home and temp paths in it are replaced with `~`, `<tmp>` and `<scratchpad>`.

## Prerequisites

- SBCL and `cl-repo`. The dependencies come from GHCR.
- **Node 24 or newer.** The backend uses `COMPUTE_UNKERNEL_NODE` if it is set, and `node` on `PATH` otherwise.
- **A guest home**, filled once by the backend's `scripts/setup.sh`. The script is the only step that downloads anything: it runs `npm ci` of the pinned host modules, fetches the pinned Alpine image (SHA-256 checked) and builds the guest disk with `sbcl=2.6.5-r0`. The script ships in the installed package:

  ```bash
  export COMPUTE_UNKERNEL_NODE=/opt/homebrew/opt/node@26/bin/node   # if the node on PATH is older than 24
  export COMPUTE_UNKERNEL_HOME=~/.cache/compute-backend-unkernel/0.2.0-261121-e69adda
  PATH=$(dirname "$COMPUTE_UNKERNEL_NODE"):$PATH \
    sh ~/.local/share/cl-repository/systems/compute-backend-unkernel/0.1.0/scripts/setup.sh "$COMPUTE_UNKERNEL_HOME"
  ```

  The package is installed by the first `cl-repo run` below. You can also run `scripts/setup.sh` from a checkout of [egao1980/compute-backend-unkernel](https://github.com/egao1980/compute-backend-unkernel). If `COMPUTE_UNKERNEL_HOME` is unset, the backend and the script both default to `$XDG_CACHE_HOME/compute-backend-unkernel/<unkernel version>/`. If the home is missing, Section 1 prints the exact `setup.sh` command to run, and the demo stops there.

## Run

```bash
cd harness/unkernel
COMPUTE_UNKERNEL_NODE=… COMPUTE_UNKERNEL_HOME=… \
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-unkernel")' \
  --eval '(harness-demos-unkernel:run-demos)' > demo-output.txt 2>&1
```

Once the packages are installed, a run takes about 15 s. Most of that is Section 6.

## What each section shows

1. **Availability.** `confinement-available-p` returns `T NIL` for the configured backend in about 160 ms, and the result is cached per backend. It also prints `probe-node` and `confinement-features`. Two broken backends follow, each with its reason string. The first points `:node` at a missing executable, and its reason names the executable and "Node 24". The second points `:home` at an empty directory, and its reason gives the exact `setup.sh` command. For both, `run-sandboxed` signals `confinement-unavailable`. The backend fails closed and never runs a command unconfined.
2. **One-shot `run-sandboxed`.** The host is `Darwin arm64`. Inside the guest, `uname -a` prints `Linux unkernel 7.3.4+ … aarch64` and `/etc/os-release` says Alpine 3.24.4. Each one-shot run (boot, run, power-off) takes about 640 ms.
3. **Isolation.** A session has the confinement `:writable (area) :readable (ro)` and leaves `outside/` unmounted. The guest's commands give these results:
   - `outside/secret.txt` does not exist in the guest, and the common parent lists only `area` and `ro`.
   - `ro/` can be read, but a write fails with `Read-only file system`, and nothing appears on the host.
   - The session's cwd is the writable area. A file the guest writes there can be read on the host.
   - The network is off by default. `nslookup` fails with `Network unreachable`, `nc` to 1.1.1.1:443 fails, and the only interface is `lo`.
   - `TMPDIR` is a fresh host directory, `stack-compute-<16 hex>`, mounted read-write. The guest sees only the mount points, not the other entries (about 5,000) in the host's temp directory. The directory is deleted at `close-sandbox`.
4. **A session.** `open-sandbox` takes about 370 ms, of which the kernel boot is about 290 ms. Five execs in the warm guest then take about 3 ms each, while a cold one-shot run takes about 640 ms. Use a session when you run more than one command.
5. **Deadline.** `session-exec` runs `sleep 30` with `:wall-clock 2`. The section prints the `handler-bind` it uses: on `sandbox-timeout` (limit 2) the handler invokes the `abort-execution` restart, and `session-exec` returns `NIL` after 2.1 s. Before the call, the host process pid is alive and one `unkernel-host.mjs` is running. Afterwards the pid is gone, `pgrep` finds no `unkernel-host.mjs` processes, and `close-sandbox` returns `NIL` because the session was already killed.
6. **SBCL inside the guest.** The guest runs `sbcl --non-interactive --eval` on three forms: a generic-arithmetic loop, a fixnum loop at `(speed 3)`, and a consing / sort / GC workload. The same code also runs compiled on the host SBCL, and each form is timed by both the guest's clock and the host's (the two agree). **CPU-bound work in the guest is much slower than native.** The backend documents 15–40x (26–34x in its README). On this Mac these loops measured **3.5–9.3x**: the fixnum loop was the fastest and the consing workload the slowest. This is the known tradeoff of the portable, no-hypervisor tier. Prefer Seatbelt, Landlock or a microVM where they exist, and use unkernel as the fallback, never the default.

## Rough edges found

Filed on [egao1980/compute-backend-unkernel](https://github.com/egao1980/compute-backend-unkernel/issues):

- The 26–34x slowdown figure in the README and docstrings does not match this run, which measured 3.5–9.3x on Apple silicon with Node 26. The figure should say which workload and host it was measured on. ([#4](https://github.com/egao1980/compute-backend-unkernel/issues/4))
