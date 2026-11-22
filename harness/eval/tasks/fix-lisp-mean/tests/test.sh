#!/bin/bash
# Verifier: sbcl --script on test-stats.lisp, which loads ./stats.lisp.
LOGS="${AGENT_EVAL_LOGS_DIR:-/logs}"
TESTS="${AGENT_EVAL_TESTS_DIR:-/tests}"
mkdir -p "$LOGS/verifier"
if sbcl --script "$TESTS/test-stats.lisp"; then
  echo 1 > "$LOGS/verifier/reward.txt"
else
  echo 0 > "$LOGS/verifier/reward.txt"
fi
