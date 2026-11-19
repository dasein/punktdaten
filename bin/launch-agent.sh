#!/usr/bin/env bash
# Open a new tmux window running a coding agent (claude or codex) in the given directory.
# Args: <agent> <dir> [window-name]
# The window name defaults to the directory name, prefixed with the first two
# letters of the agent (cl:, co:) to show which agent runs there.
set -uo pipefail

agent="${1:?usage: launch-agent.sh <claude|codex> <dir> [window-name]}"
dir="${2:-$PWD}"
name="${3:-${agent:0:2}:$(basename "$dir")}"

target=$(tmux new-window -c "$dir" -n "$name" -P -F '#{session_name}:#{window_index}' "$agent")
tmux set-option -wt "$target" @window_state idle
