#!/usr/bin/env bash
# fzf popup picker for all tmux windows across all sessions.
# Claude windows show status via @claude_state; others show a neutral indicator.
set -uo pipefail

emit_rows() {
  local now
  now=$(date +%s)
  tmux list-windows -a -F '#{session_name}:#{window_index}	#{window_name}	#{pane_current_path}' \
  | while IFS=$'\t' read -r target name path; do
      state=$(tmux show-options -qv -t "$target" @claude_state 2>/dev/null)
      at=$(tmux show-options -qv -t "$target" @claude_state_at 2>/dev/null)
      case "$state" in
        waiting) icon=$'\033[33m●\033[0m waiting' rank=0 ;;
        idle)    icon=$'\033[32m●\033[0m idle   ' rank=1 ;;
        working) icon=$'\033[31m●\033[0m working' rank=2 ;;
        *)       icon=$'\033[90m●\033[0m        ' rank=3 ;;
      esac
      if [ -n "$at" ]; then ago="$(( (now - at) / 60 ))m"; else ago=''; fi
      printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$rank" "$target" "$icon" "$name" "${path/#$HOME/~}" "$ago"
    done | sort -n
}

sel=$(emit_rows | fzf --ansi --delimiter=$'\t' --with-nth=3,4,5,6 \
  --reverse --cycle \
  --header='windows · enter: jump · ctrl-x: kill' \
  --preview='tmux capture-pane -ept {2}' \
  --preview-window='right,62%,wrap' \
  --bind="ctrl-x:execute-silent(tmux kill-window -t {2})+reload($(realpath "$0") --list)")

[ -z "$sel" ] && exit 0
target=$(printf '%s' "$sel" | cut -f2)
tmux switch-client -t "$target"
