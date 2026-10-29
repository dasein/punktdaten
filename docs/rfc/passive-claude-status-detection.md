# Passive Claude status detection for tmux-windows

Date: 2026-08-06

## Background

`bin/tmux-windows` is a lightweight fzf-based picker/status-bar tool for
switching between tmux windows and showing whether a Claude session in each
window is idle, working, or waiting for input. State is currently pushed by
Claude Code hooks (`PreToolUse`, `PostToolUse`, `Stop`, `Notification`,
`UserPromptSubmit` in `.claude/settings.json`), which shell out to
`tmux-windows --set-state <state>` and write it to the `@window_state` tmux
window option.

[Ymirke/tmux-agent-switcher](https://github.com/Ymirke/tmux-agent-switcher) is
a similar (much larger, Rust/ratatui) tool that detects agent state purely by
reading `tmux` pane metadata and `capture-pane` output — no hooks, no
wrapping, no shims. This RFC ports its detection approach into
`bin/tmux-windows` so status no longer depends on Claude settings/hooks, and
generalizes the status resolver so it doesn't silently assume every
non-shell pane is "working".

Scope: Claude and Codex. Codex is detected from its OSC pane title alone
(`detect_codex_state`): a braille spinner prefix means working, and
"Action Required" (set for approval prompts) means waiting. OpenCode is not
implemented (YAGNI).

## Design

### 1. Passive detection

Two new pieces of logic in `bin/tmux-windows`:

- `is_claude(cmd)` — true if `pane_current_command` is `claude`,
  `claude-code`, or a bare semver (`X.Y.Z`, all-digit parts) — Claude's
  native installer runs a versioned binary and tmux reports that as the
  command instead of `claude` (anthropics/claude-code#49852).

- `detect_claude_state(pane_id)` — reads `#{pane_title}` (OSC title) and the
  last 25 lines of `tmux capture-pane -pJ -t <pane_id> -S -25`:
  - **working**: title starts with a braille spinner glyph followed by a
    space. Matched via literal UTF-8 byte-prefix check (braille glyphs are
    `E2 A0`–`E2 A3` lead bytes in UTF-8), so no perl/python/unicode-range
    dependency — pure bash/grep.
  - **waiting**: a selection menu is visible — a line (after stripping
    leading whitespace and box-drawing border characters `│ ┃ ║ ╎ ┆ ┊ |`)
    starts with the cursor glyph `❯` followed by a numbered option
    (`N.` or `N)`), AND at least one other line in the captured text is also
    a numbered option line. Falls back to requiring both phrases "enter to
    select" and "esc to cancel" anywhere in the captured text, for menus that
    don't match the structural shape.
  - **idle**: neither of the above.

This replaces the entire `hooks` block in `.claude/settings.json`. Remove it;
`ANTHROPIC_*`/other settings are untouched.

### 2. Background daemon

A new `bin/tmux-windows --daemon` mode, started once via
`run-shell -b "tmux-windows --daemon"` in `.tmux.conf`. Guards against
duplicate daemons the same way `tmux-agent-switcher` does: a global tmux
option holds the running daemon's PID; on start, check whether that PID is
alive and is actually a `tmux-windows --daemon` process before spawning
another.

Loop, every 1 second:

1. `tmux list-windows -a` to enumerate windows, filter to those whose active
   pane's `pane_current_command` passes `is_claude`.
2. For each such window, compute `detect_claude_state`.
3. **Debounce**: track the last-seen raw state per window in an in-memory
   associative array (daemon lifetime only). A state change is only
   committed after appearing for 2 consecutive polls (~2s), in both
   directions (idle↔working↔waiting are all debounced the same way — no
   separate longer "settle" threshold for the idle direction, unlike the
   source's `IDLE_DEBOUNCE_POLLS` vs `BUSY_DEBOUNCE_POLLS` split). This can
   be tuned later if it flaps in practice.
4. On a **freshly committed** transition into `waiting`, run the existing
   sound-trigger logic currently inside `set_state()` (the `afplay`/`say`
   block), and write `@window_state`/`@window_state_at`.
5. On any other committed transition, just write `@window_state`/
   `@window_state_at` (no sound).

No process-tree (`ps`) fallback for "agent exited but a foreground
subprocess is still running under the pane" — Claude runs directly in the
pane in this setup, so `pane_current_command` alone is sufficient. This is a
deliberate simplification vs. `tmux-agent-switcher`'s `ProcessTree`.

`set_state()` remains callable directly (`--set-state`) for any manual/other
use, but nothing in `.claude/settings.json` calls it anymore.

### 3. Generic, honest status resolution

`resolve_state()` currently falls back to "working" for any pane whose
`pane_current_command` isn't a known shell — this was a hardcoded assumption
that non-shell == Claude. Change it to be explicit about what's actually
detected:

- Pane running Claude (`is_claude`): use the daemon-maintained
  `@window_state` (unchanged from today, just now populated by the daemon
  instead of hooks).
- Pane running a shell: idle (unchanged).
- Anything else (some other CLI, editor, etc.): no dot / untracked — we have
  no real detection for it, so don't fabricate a state. `emit_rows` and
  `status_dots` need no structural changes; an untracked pane simply doesn't
  contribute to either the picker's color-coded rank or the status-bar dot
  counts.

## Out of scope

- OpenCode detection (no current usage).
- A docked/always-visible sidebar (the fzf popup dock+preview layout stays;
  only the status semantics change, not the popup mechanics).
- Process-tree fallback for detecting an agent's foreground subprocess.
- Separate idle-vs-busy debounce thresholds.

## Rollout

1. Implement `is_claude` and `detect_claude_state` in `bin/tmux-windows`.
2. Implement `--daemon` mode with debounce and sound-on-waiting.
3. Update `resolve_state()` to the three-way (Claude / shell / untracked)
   resolution.
4. Add `run-shell -b "tmux-windows --daemon"` to `.tmux.conf`.
5. Remove the `hooks` block from `.claude/settings.json`.
6. Manually verify: open a Claude window, trigger a permission prompt
   (waiting), let it finish (idle), confirm the sound and status-bar dots
   still behave as before.
