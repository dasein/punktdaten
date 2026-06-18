#!/usr/bin/env bash
# Open a new tmux window running Claude in the given directory.
# Args: <dir> [window-name]
set -uo pipefail

dir="${1:-$PWD}"
name="${2:-$(basename "$dir")}"

target=$(tmux new-window -c "$dir" -n "$name" -P -F '#{session_name}:#{window_index}' "claude")
tmux set-option -wt "$target" @window_state idle
