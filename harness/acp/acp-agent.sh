#!/bin/sh
# Launch the demo ACP agent on stdin / stdout: the `command` of Zed's
# agent_servers entry, and the child the headless client spawns.
# Dependencies come from cl-repo (GHCR). `cl-repo run -- sbcl --script` puts
# its --eval before the runtime options, so SBCL prints its banner on stdout
# (cl-repository issue); stdout must carry ACP messages only. So cl-repo run
# only writes the source registry form to a file, and SBCL then runs
# agent-main.lisp with --script, which reads it (ACP_DEMO_REGISTRY).
# ACP_DEMO_EXTRA_TREE: an unpublished checkout to put first (optional).
here=$(cd "$(dirname "$0")" && pwd)
PATH="$HOME/.local/bin:/opt/homebrew/bin:$HOME/.roswell/bin:$PATH"
export PATH
if [ -n "$ACP_DEMO_EXTRA_TREE" ]; then
  CL_SOURCE_REGISTRY="(:source-registry (:tree \"$ACP_DEMO_EXTRA_TREE\") :inherit-configuration)"
  export CL_SOURCE_REGISTRY
fi
ACP_DEMO_REGISTRY="${TMPDIR:-/tmp}/harness-demos-acp-registry.$$.sexp"
export ACP_DEMO_REGISTRY
cd "$here" || exit 1
cl-repo run --quiet -- sbcl --non-interactive --no-userinit \
  --eval '(with-open-file (o (uiop:getenv "ACP_DEMO_REGISTRY") :direction :output :if-exists :supersede) (with-standard-io-syntax (prin1 asdf::*source-registry-parameter* o)))' \
  >&2 || exit 1
exec sbcl --script "$here/agent-main.lisp"
