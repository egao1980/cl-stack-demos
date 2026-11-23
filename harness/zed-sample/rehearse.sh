#!/usr/bin/env bash
# Rehearse the Zed demo headless: clone cl-stack-sample at demo/zed-seeded-bug into
# a fresh directory, cl-repo sync it, then run the scripted ACP client
# (harness-demos-zed-sample:rehearse) against cl-stack-code --acp.
# Writes rehearsal/output.txt and rehearsal/wire-log.txt (paths redacted).
#   HARNESS_DEMOS_BASE_URL / HARNESS_DEMOS_API_KEY / HARNESS_DEMOS_MODEL: the model
#   ZED_SAMPLE_DIR: where to clone (default: a new temp dir)
#   ZED_SAMPLE_REF (default demo/zed-seeded-bug), ZED_SAMPLE_LAUNCHER (default cl-stack-code)
#   ZED_SAMPLE_RAW_LOG: keep the unfiltered SBCL output there (default: kept as
#   rehearsal/output.raw only when the run fails, so a load error is never lost)
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
export PATH="$HOME/.local/bin:$HOME/.roswell/bin:/opt/homebrew/bin:$PATH"
: "${HARNESS_DEMOS_BASE_URL:?}" "${HARNESS_DEMOS_API_KEY:?}" "${HARNESS_DEMOS_MODEL:?}"
export OPENAI_BASE_URL="$HARNESS_DEMOS_BASE_URL" OPENAI_API_KEY="$HARNESS_DEMOS_API_KEY" \
       OPENAI_MODEL="$HARNESS_DEMOS_MODEL"
export OPENAI_TIMEOUT="${OPENAI_TIMEOUT:-600}" OPENAI_STREAM_IDLE_TIMEOUT="${OPENAI_STREAM_IDLE_TIMEOUT:-300}"
dir="${ZED_SAMPLE_DIR:-$(mktemp -d "${TMPDIR:-/tmp}/zed-sample.XXXXXX")/cl-stack-sample}"
git clone -q --branch "${ZED_SAMPLE_REF:-demo/zed-seeded-bug}" https://github.com/egao1980/cl-stack-sample "$dir"
(cd "$dir" && cl-repo sync >/dev/null 2>&1) || { echo "cl-repo sync failed in $dir" >&2; exit 1; }
export ZED_SAMPLE_PROJECT="$dir" ZED_SAMPLE_WIRE_LOG="$here/rehearsal/wire-log.txt"
mkdir -p "$here/rehearsal"
cd "$here"
status=0
cl-repo run -- sbcl --non-interactive --no-userinit --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-zed-sample")' \
  --eval '(uiop:quit (if (harness-demos-zed-sample:rehearse) 0 1))' \
  > rehearsal/output.raw 2>&1 || status=$?
# keep the client's lines and the result; drop compile noise (but not a
# compiler or reader ERROR and its context); redact
perl -ne '$e = 12 if /caught ERROR|READ error/; if ($e > 0) { $e--; print; next } print unless /^;|^WARNING|^$|STYLE-WARNING|^  *$/' rehearsal/output.raw \
  | perl -pe 's/\Q$ENV{ZED_SAMPLE_PROJECT}\E/<project>/g; s{/private/tmp/claude-\d+/[^"\\\s]*}{<tmp>}g; s/\Q$ENV{HOME}\E/~/g; s/\b\Q$ENV{USER}\E\b/<user>/g; s/[\w.+-]+\@[\w-]+(\.[\w-]+)+/<email>/g' > rehearsal/output.txt
if [[ -n "${ZED_SAMPLE_RAW_LOG:-}" ]]; then mv -f rehearsal/output.raw "$ZED_SAMPLE_RAW_LOG"
elif [[ $status -eq 0 ]]; then rm -f rehearsal/output.raw; fi
(cd "$dir" && git diff) | perl -pe 's/\Q$ENV{HOME}\E/~/g' > rehearsal/fix.diff
tail -20 rehearsal/output.txt
exit $status
