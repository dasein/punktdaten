# Source shared configuration for both bash and zsh
if [ -f ~/.shellrc ]; then
    . ~/.shellrc
fi

# Bash-specific history settings
shopt -s histappend
PROMPT_COMMAND="history -a; history -r"

# Bash-specific shell options
shopt -s expand_aliases

# Bash prompt setup
prompt() {
  # Zenburn colors
  local YELLOW="\[\e[0;33m\]"
  local BLUE="\[\e[0;36m\]"
  local GREEN="\[\e[0;32m\]"
  local RESET="\[\e[0m\]"

  # Show host only if SSH or in tmux
  local USRHOST=""
  if [[ -n "$SSH_TTY" ]]; then
    USRHOST="${YELLOW}\h${RESET}:"
  fi

  # Set XTerm/tmux window title (omit in Emacs)
  local TITLEBAR=""
  if [[ -z "$EMACS" && -z "$INSIDE_EMACS" ]]; then
    TITLEBAR="\[\e]0;\u@\h: \W \$(git-branch)\a\]"
  fi

  # Actual prompt
  PS1="${TITLEBAR}${USRHOST}${BLUE}[\W]${GREEN}\$(git-branch)${RESET}\$ "
}
prompt

# Bash extras
if [ -f ~/.bash.extra ]; then
    . ~/.bash.extra
fi
