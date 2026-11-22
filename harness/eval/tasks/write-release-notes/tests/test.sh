#!/bin/bash
# Verifier: compare notes/release.txt byte for byte with the expected text.
LOGS="${AGENT_EVAL_LOGS_DIR:-/logs}"
mkdir -p "$LOGS/verifier"
expected="$(printf '%s\n' "agent-eval 0.1.0" "Terminal-Bench tasks on eval-protocol" "nop, oracle, cl-stack-code" | od -An -tx1)"
if [ -f notes/release.txt ] && [ "$(od -An -tx1 < notes/release.txt)" = "$expected" ]; then
  echo "notes/release.txt: ok"
  echo 1 > "$LOGS/verifier/reward.txt"
else
  echo "notes/release.txt: missing or different"
  [ -f notes/release.txt ] && cat -vet notes/release.txt 2>/dev/null | head -5
  echo 0 > "$LOGS/verifier/reward.txt"
fi
