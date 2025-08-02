# Source shared configuration for both bash and zsh
if [ -f ~/.shellrc ]; then
    . ~/.shellrc
fi

# Key bindings
bindkey -e  # Use emacs key bindings

# history settings
setopt HIST_IGNORE_DUPS        # ignore duplicate entries
setopt HIST_EXPIRE_DUPS_FIRST  # expire duplicate entries first when trimming history
setopt HIST_FIND_NO_DUPS       # do not find duplicates in history
setopt HIST_IGNORE_SPACE       # ignore commands that start with space
setopt HIST_SAVE_NO_DUPS       # do not save duplicates
setopt SHARE_HISTORY           # share history between all sessions
setopt APPEND_HISTORY          # append to history file
setopt INC_APPEND_HISTORY      # save commands are added to the history immediately

# options
setopt CORRECT                 # auto correct mistakes
setopt AUTO_LIST               # automatically list choices on ambiguous completion
setopt AUTO_MENU               # automatically use menu completion
setopt ALWAYS_TO_END           # move cursor to end if word had one match
setopt PROMPT_SUBST            # Zsh prompt setup with git integration

# Enable completion system
autoload -Uz compinit
compinit

# Custom completion
zstyle ':completion:*:make:*:targets' call-command true
zstyle ':completion:*:*:make:*' tag-order 'targets'

# Zsh-specific prompt function
prompt() {
  # Zenburn colors for zsh
  local YELLOW='%F{yellow}'
  local BLUE='%F{cyan}'
  local GREEN='%F{green}'
  local RESET='%f'

  # Show host only if SSH or in tmux
  local USRHOST=""
  if [[ -n "$SSH_TTY" ]]; then
    USRHOST="${YELLOW}%m${RESET}:"
  fi

  # Set terminal title
  precmd() {
    print -Pn "\e]0;%n@%m: %~ $(git-branch)\a"
  }

  # Actual prompt
  PROMPT="${USRHOST}${BLUE}[%1~]${GREEN}\$(git-branch)${RESET}%# "
}
prompt

# Zsh extras
if [ -f ~/.zsh.extra ]; then
    . ~/.zsh.extra
fi
