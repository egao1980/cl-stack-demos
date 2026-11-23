#!/bin/bash
# The terminal demo: cl-stack-code's interactive mode on cl-stack-sample.
# The key comes from Zed's settings (cl-stack-code's env) and never appears
# on screen or in this file.
export PATH="$HOME/.local/bin:$HOME/.roswell/bin:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin"
ANTHROPIC_API_KEY="$(sed -n 's/.*"ANTHROPIC_API_KEY": *"\([^"]*\)".*/\1/p' "$HOME/.config/zed/settings.json" | head -1)"
export ANTHROPIC_API_KEY
cd /Users/Shared/cl-demo/cl-stack-sample || exit 1
clear
exec "$HOME/Projects/cl-workspace/.lisp-workspace/scripts/cl-stack-code.sh" --model anthropic:claude-opus-5-5
