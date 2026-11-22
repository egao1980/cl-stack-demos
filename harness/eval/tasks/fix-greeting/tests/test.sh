#!/bin/bash
# Verifier. Runs in the work dir. $AGENT_EVAL_LOGS_DIR is set by agent-eval's
# environments; a plain Terminal-Bench container has /logs.
LOGS="${AGENT_EVAL_LOGS_DIR:-/logs}"
mkdir -p "$LOGS/verifier"
ok=1
check() { # expected, args...
  local want="$1"; shift
  local got; got="$(bash greet.sh "$@" 2>&1)"
  if [ "$got" = "$want" ]; then echo "ok:   greet.sh $* -> $got"; else echo "FAIL: greet.sh $* -> '$got', want '$want'"; ok=0; fi
}
check "Hello, Lisp!" Lisp
check "Hello, world!"
echo "$ok" > "$LOGS/verifier/reward.txt"
