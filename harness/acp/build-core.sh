#!/bin/sh
# Build the demo agent as one SBCL executable (default: ./acp-demo-agent, or
# $1), so an editor launches it in well under a second: the dependencies come
# from cl-repo (GHCR) and are compiled and loaded here, once. Zed's
# agent_servers `command` points at the executable; acp-agent.sh is the slow
# path (cl-repo run + compile at every launch) and needs no build.
# ACP_DEMO_EXTRA_TREE: an unpublished checkout to put first (optional).
set -e
here=$(cd "$(dirname "$0")" && pwd)
out=${1:-$here/acp-demo-agent}
PATH="$HOME/.local/bin:/opt/homebrew/bin:$HOME/.roswell/bin:$PATH"
export PATH
if [ -n "$ACP_DEMO_EXTRA_TREE" ]; then
  CL_SOURCE_REGISTRY="(:source-registry (:tree \"$ACP_DEMO_EXTRA_TREE\") :inherit-configuration)"
  export CL_SOURCE_REGISTRY
fi
ACP_DEMO_OUT=$out
export ACP_DEMO_OUT
cd "$here"
cl-repo run --quiet -- sbcl --non-interactive --no-userinit \
  --eval '(require "asdf")' \
  --eval '(asdf:load-system "harness-demos-acp")' \
  --eval '(sb-ext:save-lisp-and-die (uiop:getenv "ACP_DEMO_OUT") :executable t :toplevel (function harness-demos-acp:agent-toplevel) :save-runtime-options nil)'
echo "built $out" >&2
