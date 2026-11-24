# imports demo: the python-to-cl libraries, installed from GHCR

`harness-demos-imports` uses the third-party libraries that the [python-to-cl](https://egao1980.github.io/python-to-cl/) pages recommend. Each one does the job its page names. Before egao1980/cl-stack-systems#79, none of them could be installed with cl-repo:
- jose 0.1.0 was on GHCR, but its dependencies cl-json and assoc-utils were not (cl-stack-systems#75);
- the first tier of cl-stack-systems#76 was not imported at all (`cl-repo sync`: "system lparallel is not in the registry").

#79 imported 53 libraries. cl-stack-systems#84 republished frugal-uuid under its primary name, so cl-smtp installs too (egao1980/cl-repository#184).

The code is in [`src/demo.lisp`](src/demo.lisp). [`demo-output.txt`](demo-output.txt) is the output of the last run: macOS arm64, SBCL 2.6.9, every dependency installed from `ghcr.io/egao1980/cl-systems` by `cl-repo run`, with temp paths shown as `<tmp>`. The run makes 31 checks, and `run-demos` returns `NIL` if one fails or a section stops on an error.

## Run

```bash
cd harness/imports
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-imports")' \
  --eval '(uiop:quit (if (harness-demos-imports:run-demos) 0 1))' > demo-output.txt 2>&1
```

Then cut the compile log above the first banner. The demo needs no network after the sync, and no servers.

## Sections

| # | Library | Python counterpart | What it checks |
|---|---|---|---|
| 1 | jose, cl-json, assoc-utils (#75) | PyJWT | HS256 `encode` / `decode` round trip; another key signals `jws-verification-error`; jose's two dependencies on their own |
| 2 | lparallel | multiprocessing, concurrent.futures | `pmap`, `preduce` and three `future`s on a 4-worker kernel |
| 3 | cl-csv | csv | `write-csv` with a quoted comma and a doubled quote, read back equal |
| 4 | 3bmd + colorize + html-encode | markdown, mistune | a heading, a colorized fenced Lisp block, a table |
| 5 | cl-pdf (egao1980 fork), cl-pdf-parser, cl-typesetting | ReportLab, pypdf | a two-page PDF that cl-pdf-parser reads back as 2 pages; a non-ASCII name is written as `/Caf#E9`; a cl-typesetting paragraph. Upstream cl-pdf could not write any PDF without cl-pdf-parser (`EXTENDED-ASCII-P` undefined); egao1980/cl-pdf#1 fixes it |
| 6 | opticl | Pillow | a 4×3 RGB image written as PNG and read back with its pixels |
| 7 | function-cache | functools.lru_cache | `defcached`: three calls, one body run |
| 8 | plump, clss, lquery | BeautifulSoup | `li.x` with clss, `$ "li" (text)` with lquery |
| 9 | machine-state, check-it, parcom | psutil, hypothesis, parsec | CPU cores, generated integers and a property, a char parser |
| 10 | cl-smtp, cl-redis, cl-excel, legit, cl-progress-bar | smtplib, redis-py, openpyxl, GitPython, tqdm | loaded only: they need a server, a workbook or a repository |

The second tier of #76 (MAGICL, Lisp-Stat, CLOG, common-lisp-jupyter, cl-rdkafka, cl-ssh, nodgui, py4cl2, cl-random-forest, psychiq) is not imported yet: egao1980/cl-stack-systems#78.
