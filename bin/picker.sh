#!/usr/bin/env bash
# fzf popup picker for all tmux windows across all sessions.
# --status mode: print tmux status bar dot summary instead of launching picker.
set -uo pipefail

SELF=$(realpath "$0")

set_state() {
  local state="$1"
  tmux set-option -pt "$TMUX_PANE" @claude_state "$state"
  tmux set-option -pt "$TMUX_PANE" @claude_state_at "$(date +%s)"
  if [ "$state" = "working" ]; then
    tmux set-option -pt "$TMUX_PANE" @claude_ever_worked 1
  fi
  if [ "$state" = "waiting" ]; then
    {
      local f
      f=$(ls ~/wow/*.wav 2>/dev/null | shuf -n1)
      if [ -n "$f" ]; then
        afplay "$f"
      elif command -v fortune &>/dev/null; then
        fortune -s | say
      else
        say "Please configure your notifications"
      fi
    } &
  fi
}

# Resolve effective state for a window given its @claude_state and foreground command.
# Claude windows use @claude_state. Others: working if a non-shell process is
# in the foreground, idle otherwise.
resolve_state() {
  local claude_state="$1" cmd="$2" target="$3"
  if [ -n "$claude_state" ]; then
    # If claude exited, the pane reverts to a shell — stale state, treat as idle
    case "$cmd" in
      bash|zsh|sh|fish|'') printf 'idle'; return ;;
    esac
    # Only show waiting if the session has had real user interaction —
    # startup notifications (e.g. /doctor) fire before any prompt is submitted
    if [ "$claude_state" = "waiting" ]; then
      local ever_worked
      ever_worked=$(tmux show-options -qvp -t "$target" @claude_ever_worked 2>/dev/null)
      [ -z "$ever_worked" ] && { printf 'idle'; return; }
    fi
    printf '%s' "$claude_state"
    return
  fi
  case "$cmd" in
    bash|zsh|sh|fish|claude|'') printf 'idle' ;;
    *)                          printf 'working' ;;
  esac
}

emit_rows() {
  local now
  now=$(date +%s)
  tmux list-windows -a -F '#{session_name}:#{window_index}|#{window_name}|#{pane_current_path}|#{@claude_state}|#{pane_current_command}' \
  | while IFS='|' read -r target name path claude_state cmd; do
      state=$(resolve_state "$claude_state" "$cmd" "$target")
      at=$(tmux show-options -qvp -t "$target" @claude_state_at 2>/dev/null)
      case "$state" in
        waiting) icon=$'\033[31m●\033[0m waiting' rank=0 ;;
        working) icon=$'\033[33m●\033[0m working' rank=1 ;;
        *)       icon=$'\033[32m●\033[0m idle   ' rank=2 ;;
      esac
      if [ -n "$at" ]; then ago="$(( (now - at) / 60 ))m"; else ago=''; fi
      printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$rank" "$target" "$icon" "$name" "${path/#$HOME/~}" "$ago"
    done \
  | sort -n
}

status_dots() {
  local idle=0 waiting=0 working=0
  while IFS='|' read -r target name path claude_state cmd; do
    state=$(resolve_state "$claude_state" "$cmd" "$target")
    case "$state" in
      idle)    idle=$((idle + 1)) ;;
      waiting) waiting=$((waiting + 1)) ;;
      working) working=$((working + 1)) ;;
    esac
  done < <(tmux list-windows -a -F '#{session_name}:#{window_index}|#{window_name}|#{pane_current_path}|#{@claude_state}|#{pane_current_command}')

  local out=""
  [ "$idle"    -gt 0 ] && out="${out}#[fg=#7F9F7F]● ${idle}#[default] "
  [ "$waiting" -gt 0 ] && out="${out}#[fg=#CC9393]● ${waiting}#[default] "
  [ "$working" -gt 0 ] && out="${out}#[fg=#F0DFAF]● ${working}#[default]"
  printf '%s' "${out% }"
}

case "${1:-}" in
  --status)    status_dots;    exit 0 ;;
  --rows)      emit_rows;      exit 0 ;;
  --set-state) set_state "$2"; exit 0 ;;
esac

sel=$(emit_rows | fzf --ansi --delimiter=$'\t' --with-nth=3,4,5,6 \
  --reverse --cycle \
--header='windows · enter: jump · ctrl-x: kill' \
  --preview='tmux capture-pane -ept {2}' \
  --preview-window='right,62%,wrap' \
  --bind="ctrl-x:execute-silent(tmux kill-window -t {2})+reload($SELF --rows)")

[ -z "$sel" ] && exit 0
target=$(printf '%s' "$sel" | cut -f2)
tmux switch-client -t "$target"
