# MIME demo: nested multiparts, header names, browser form-data names

`harness-demos-mime-multipart` is a tour of **mime-protocol 0.3.1** from GHCR, the MIME layer under mail-protocol and cl-stack-http's multipart forms. It shows the fix for [mime-protocol#9](https://github.com/egao1980/mime-protocol/issues/9): a part whose content ended in CRLF, which every nested multipart does, made `parse-mime` signal `multipart boundary desync`, so the layout every mail client sends (text with an HTML alternative, plus an attachment) did not round-trip. It also shows the canonical header names `print-mime` now writes (item 2 of [mail-protocol#3](https://github.com/egao1980/mail-protocol/issues/3)). Sections 6 and 7 show the fix for [mime-protocol#14](https://github.com/egao1980/mime-protocol/issues/14) (0.3.1): a browser's multipart/form-data part headers, UTF-8 names and a backslash in a file name, which 0.3.0 read as `PrÃ©lude.svg` and `ab.svg`.

Every result with a known answer is checked (46 checks, 35 when uv is missing), and the summary line reports the mismatches. Everything is in [`src/demo.lisp`](src/demo.lisp). [`demo-output.txt`](demo-output.txt) is the output of the last run, on macOS arm64 with SBCL 2.6.9, home and temp paths replaced by `~` and `<tmp>`.

## Run

```bash
cd harness/mime-multipart
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-mime-multipart")' \
  --eval '(harness-demos-mime-multipart:run-demos)' > demo-output.txt 2>&1
```

Sections 5 and 7 need [uv](https://docs.astral.sh/uv/) on `PATH`. Section 5 runs Python's standard `email` package (`uv run --no-project python -I -c ...`, no package download). Section 7 runs python-multipart 0.0.34 and werkzeug 3.1.9 (`uv run --no-project --with python-multipart==0.0.34 --with werkzeug==3.1.9 python -I -c ...`, pinned so their checked readings stay the same). Without uv each prints a note and is skipped. `run-demos` returns `NIL` when a check fails or a section stops on an error, and CI then exits 1.

## What each section shows

1. **The mail layout, printed.** `multipart/mixed(multipart/alternative(text/plain, text/html), application/pdf)` built with `make-multipart-entity`, `make-text-entity` and `make-binary-entity`, headers added with `set-header` (stored lowercase), fixed boundaries so the text is the same on every run. The whole `print-mime` output: 730 octets, every line ending in CRLF, names printed as `From`, `Message-ID`, `MIME-Version`, `Content-Transfer-Encoding`.
2. **Parsed back.** `parse-mime` of that text gives the same tree as the one built, contents included. The text part keeps its own final CRLF (0.2.0 cut it, then lost the boundary), and print → parse → print gives the same 730 octets.
3. **Edge cases.** An inner boundary `b2` with the outer `b` as its prefix; part contents `""`, `"x"`, a lone CRLF, blank lines inside, a `--` line that is not the boundary, each round-tripped; an LF-only message; a preamble, a part with no headers and an epilogue, kept through print and parse.
4. **Header names.** `canonical-header-name` for eight names (`Content-MD5`, `DKIM-Signature`, `X-Mailer`, ...), and `header-value` lookups that stay case-insensitive.
5. **Cross-read with Python's `email` package, both ways.** Python reads the section 1 text with `email.message_from_bytes(policy=default)` and sees the same tree and the same contents, CRLFs included. Python writes an `EmailMessage` with `set_content`, `add_alternative` and `add_attachment` (`as_bytes(policy=SMTP)`), and `parse-mime` reads its layout, text, attachment octets and Subject.
6. **A browser's form-data part headers (#14).** The body Chrome and Firefox post for a text input named `имя` holding `Prélude`, file inputs holding `Prélude.svg`, `a\b.svg` (a legal file name on macOS and Linux) and `say "hi".svg`, and a `FormData.append("a\nb", "x")` field, built from raw octets the way the HTML multipart/form-data encoding writes them: names in UTF-8, no `filename*`, only `"` CR LF escaped as `%22 %0D %0A`. The header lines are printed with every octet above 7F as `\xHH`. `parse-mime` gives back the names the user typed or chose: `имя`, `Prélude.svg`, `a\b.svg`, `say "hi".svg`, `a\r\nb`. Next to them, the two readings 0.3.0 used: the UTF-8 octets of `Prélude.svg` as ISO-8859-1 (`PrÃ©lude.svg`), and `attachment; filename="a\b.svg"`, where RFC 822 quoted-pairs still apply (`ab.svg`). A field whose octets are not UTF-8 (`caf\xE9.txt`) falls back to ISO-8859-1 (`café.txt`). Our own encoders: `format-form-data-disposition` written as UTF-8, as http-protocol's multipart sends it, still parses through its `filename*`, and `make-form-data` + `print-mime` now quote a backslash and a quote the browser way and parse back to the same names.
7. **The same form read by python-multipart and werkzeug.** The section 6 body goes to python-multipart 0.0.34 (header octets decoded as UTF-8, as Starlette does) and to werkzeug 3.1.9's `MultipartDecoder`, and the table puts their names next to mime-protocol's. All three read the UTF-8 names alike. python-multipart keeps the backslash and leaves `%22 %0D %0A` to the application. werkzeug decodes `%22` and keeps `%0D %0A`, and reads `a\b.svg` as `ab.svg`: it unescapes quoted-pairs, as mime-protocol 0.3.0 did.

## Issues found

- [mime-protocol#13](https://github.com/egao1980/mime-protocol/issues/13): a part with no `Content-Type` header parses with `mime-content-type` `NIL`, so `media-type-type` on it has no applicable method. RFC 2045 makes it `text/plain; charset=us-ascii`, and Python's `email` says `text/plain`. Section 3 prints the type as `(no Content-Type)`.
- Found earlier by the services review, open: [mime-protocol#12](https://github.com/egao1980/mime-protocol/issues/12), `set-header` / `print-mime` accept CR or LF in a header value (header injection).
- Fixed in 0.3.1, shown in sections 6 and 7: [mime-protocol#14](https://github.com/egao1980/mime-protocol/issues/14), a browser's UTF-8 `filename` read as ISO-8859-1 and a backslash in it taken as a quoted-pair ([PR #15](https://github.com/egao1980/mime-protocol/pull/15)).
- Found while fixing #14: [mime-protocol#16](https://github.com/egao1980/mime-protocol/issues/16), `parse-mime` of a `print-mime` string signals when header text is outside ISO-8859-1 (a filename with an em dash, say). The demo parses octets, as a server does, so it does not hit this.
