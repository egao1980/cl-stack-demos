#!/usr/bin/env bash
# cl-stack-code --acp with a wire log: the `command` of the Zed agent_servers
# entry in this demo, and the child the headless rehearsal spawns.
# Every JSON-RPC line is appended to $ZED_SAMPLE_WIRE_LOG with "-> " (editor
# to agent) or "<- " (agent to editor). $ZED_SAMPLE_REDACT (the project
# directory) becomes <project> and $HOME becomes ~. Arguments go to the
# launcher ($CL_STACK_CODE_LAUNCHER, default: cl-stack-code on PATH).
# The key is never on the wire; it comes from the environment only. E-mail
# addresses (git output) become <email>.
set -uo pipefail
log="${ZED_SAMPLE_WIRE_LOG:?set ZED_SAMPLE_WIRE_LOG}"
launcher="${CL_STACK_CODE_LAUNCHER:-cl-stack-code}"
export ZED_SAMPLE_REDACT="${ZED_SAMPLE_REDACT:-}"
mark() {  # mark PREFIX: redact and prefix each line, unbuffered
  perl -ne 'BEGIN { $| = 1; $p = shift @ARGV; $r = $ENV{ZED_SAMPLE_REDACT}; $h = $ENV{HOME}; $u = $ENV{USER} // "" }
            s/\Q$r\E/<project>/g if length $r;
            s/\Q$h\E/~/g if length $h;
            s/\b\Q$u\E\b/<user>/g if length $u;
            s/[\w.+-]+\@[\w-]+(\.[\w-]+)+/<email>/g;
            print "$p $_"' -- "$1" >>"$log"
}
tee >(mark '->') | "$launcher" "$@" | tee >(mark '<-')
