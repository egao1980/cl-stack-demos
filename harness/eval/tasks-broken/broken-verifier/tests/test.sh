#!/bin/bash
# A broken verifier on purpose: it checks the file and exits 0, but never
# writes /logs/verifier/reward.txt or reward.json, so agent-eval signals
# REWARD-MISSING.
if [ -f done.txt ]; then echo "done.txt is there"; else echo "done.txt is missing"; fi
exit 0
