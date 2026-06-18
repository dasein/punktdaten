#!/usr/bin/env bash
# Open a new tmux window running Claude in the given directory.
# Args: <dir> [window-name]
set -uo pipefail

dir="${1:-$PWD}"
name="${2:-$(basename "$dir")}"

tmux new-window -c "$dir" -n "$name" "claude"
