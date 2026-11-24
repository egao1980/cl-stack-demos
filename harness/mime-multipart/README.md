# MIME demo: nested multiparts and header names

`harness-demos-mime-multipart` is a tour of **mime-protocol 0.3.0** from GHCR, the MIME layer under mail-protocol and cl-stack-http's multipart forms. It shows the fix for [mime-protocol#9](https://github.com/egao1980/mime-protocol/issues/9): a part whose content ended in CRLF, which every nested multipart does, made `parse-mime` signal `multipart boundary desync`, so the layout every mail client sends (text with an HTML alternative, plus an attachment) did not round-trip. It also shows the canonical header names `print-mime` now writes (item 2 of [mail-protocol#3](https://github.com/egao1980/mail-protocol/issues/3)).

Every result with a known answer is checked (34 checks, 27 when uv is missing), and the summary line reports the mismatches. Everything is in [`src/demo.lisp`](src/demo.lisp). [`demo-output.txt`](demo-output.txt) is the output of the last run, on macOS arm64 with SBCL 2.6.9, home and temp paths replaced by `~` and `<tmp>`.

## Run

```bash
cd harness/mime-multipart
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-mime-multipart")' \
  --eval '(harness-demos-mime-multipart:run-demos)' > demo-output.txt 2>&1
```

Section 5 needs [uv](https://docs.astral.sh/uv/) on `PATH` and runs Python's standard `email` package (`uv run --no-project python -I -c ...`, no package download). Without uv it prints a note and is skipped. `run-demos` returns `NIL` when a check fails or a section stops on an error, and CI then exits 1.

## What each section shows

1. **The mail layout, printed.** `multipart/mixed(multipart/alternative(text/plain, text/html), application/pdf)` built with `make-multipart-entity`, `make-text-entity` and `make-binary-entity`, headers added with `set-header` (stored lowercase), fixed boundaries so the text is the same on every run. The whole `print-mime` output: 730 octets, every line ending in CRLF, names printed as `From`, `Message-ID`, `MIME-Version`, `Content-Transfer-Encoding`.
2. **Parsed back.** `parse-mime` of that text gives the same tree as the one built, contents included. The text part keeps its own final CRLF (0.2.0 cut it, then lost the boundary), and print → parse → print gives the same 730 octets.
3. **Edge cases.** An inner boundary `b2` with the outer `b` as its prefix; part contents `""`, `"x"`, a lone CRLF, blank lines inside, a `--` line that is not the boundary, each round-tripped; an LF-only message; a preamble, a part with no headers and an epilogue, kept through print and parse.
4. **Header names.** `canonical-header-name` for eight names (`Content-MD5`, `DKIM-Signature`, `X-Mailer`, ...), and `header-value` lookups that stay case-insensitive.
5. **Cross-read with Python's `email` package, both ways.** Python reads the section 1 text with `email.message_from_bytes(policy=default)` and sees the same tree and the same contents, CRLFs included. Python writes an `EmailMessage` with `set_content`, `add_alternative` and `add_attachment` (`as_bytes(policy=SMTP)`), and `parse-mime` reads its layout, text, attachment octets and Subject.

## Issues found

- [mime-protocol#13](https://github.com/egao1980/mime-protocol/issues/13): a part with no `Content-Type` header parses with `mime-content-type` `NIL`, so `media-type-type` on it has no applicable method. RFC 2045 makes it `text/plain; charset=us-ascii`, and Python's `email` says `text/plain`. Section 3 prints the type as `(no Content-Type)`.
- Found earlier by the services review, open: [mime-protocol#12](https://github.com/egao1980/mime-protocol/issues/12), `set-header` / `print-mime` accept CR or LF in a header value (header injection).
