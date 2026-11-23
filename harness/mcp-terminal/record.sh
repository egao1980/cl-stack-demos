#!/bin/bash
# Record the MCP terminal demo: cl-stack-code in iTerm2 on cl-stack-sample with
# a project .mcp.json (git MCP server via uvx). Input goes only to the demo
# window's own session (never System Events keystrokes).
set -u
REPO=/Users/Shared/cl-demo/mcp/cl-stack-sample
RUN=/Users/Shared/cl-demo/run-agent-mcp.sh
PROMPT="the tests fail on this branch: use the git MCP server to find the commit that broke them, then fix the bug"
RX=0 RY=25 RW=1600 RH=900
out=/Users/Shared/cl-demo/rec-mcp-$(date +%Y%m%d-%H%M%S); mkdir -p "$out"
trap 'exit 130' INT TERM
# reset: the fix, the trust answer (the user settings file did not exist before)
git -C "$REPO" checkout -q -- src tests agent
rm -f "$HOME/.config/cl-stack-code/settings.json"
scale=$(system_profiler SPDisplaysDataType 2>/dev/null | awk '/Resolution:/ { px = $2 } /UI Looks like:/ { pt = $4 } END { if (px && pt) printf "%d", px / pt }'); scale=${scale:-1}
perl -e '$SIG{INT} = "DEFAULT"; exec @ARGV' \
  ffmpeg -hide_banner -loglevel error -f avfoundation -capture_cursor 0 -framerate 15 -i "0:none" \
  -vf "crop=$((RW*scale)):$((RH*scale)):$((RX*scale)):$((RY*scale)),scale=$RW:$RH" \
  -t 600 -c:v libx264 -preset ultrafast -crf 23 -pix_fmt yuv420p \
  "$out/raw.mov" </dev/null > "$out/ffmpeg.log" 2>&1 &
rec=$!; t0=$(perl -MTime::HiRes=time -e 'printf "%.1f", time')
ev() { printf "%s\t%s\n" "$(perl -MTime::HiRes=time -e "printf '%.1f', time - $t0")" "$1" >> "$out/events.tsv"; }
W=$(osascript -e "tell application \"iTerm2\"
  set w to (create window with default profile command \"$RUN\")
  set bounds of w to {$RX, $RY, $((RX+RW)), $((RY+RH))}
  return id of w
end tell")
say() { osascript -e "tell application \"iTerm2\" to tell current session of window id $W to write text \"$1\" newline no"; }
enter() { osascript -e "tell application \"iTerm2\" to tell current session of window id $W to write text (character id 13) newline no"; }
screen() { osascript -e "tell application \"iTerm2\" to tell current session of window id $W to get contents" 2>/dev/null; }
waitfor() { for _ in $(seq 1 ${2:-120}); do screen | grep -q "$1" && return 0; sleep 1; done; return 1; }
ev window
waitfor "Yes, trust it in this project" 90 || { echo "no trust prompt"; }
ev trust-prompt; sleep 3; say 1; ev trusted
sleep 3; say "/mcp"; sleep 0.8; enter; ev mcp; sleep 5
say "$PROMPT"; sleep 1.2; enter; ev prompt
n=0
for i in $(seq 1 200); do
  c=$(screen)
  if echo "$c" | grep -q "> 1\. Allow once"; then n=$((n+1)); ev "approval-$n"; sleep 2.5; say 1; sleep 2; continue; fi
  if ! echo "$c" | grep -q "Working"; then sleep 2; screen | grep -q "Working" || break; fi
  sleep 1
done
ev turn-end; sleep 7
kill -INT $rec; wait $rec 2>/dev/null
screen > "$out/final-screen.txt"
osascript -e "tell application \"iTerm2\" to close window id $W" >/dev/null 2>&1
ffmpeg -y -loglevel error -i "$out/raw.mov" -vf "fps=15" -c:v libx264 -crf 28 -pix_fmt yuv420p -movflags +faststart "$out/mcp-demo.mp4"
d=$(ffprobe -v error -show_entries format=duration -of csv=p=0 "$out/mcp-demo.mp4"); echo "$out dur=${d}s approvals=$n"
cat "$out/events.tsv"
