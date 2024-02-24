# Default Paths
SPATH="/sbin:/usr/sbin:/usr/local/sbin"
if [ -z "$DEF_PATH" ]; then
  DEF_PATH=/usr/local/bin:$HOME/bin:$HOME/work/go/bin:$SPATH:$PATH
fi

# OS specific env builder
OS=$(uname -a | awk '{print $1}')
function buildenv
{
  case $OS in
    Darwin)
      export PATH=/opt/homebrew/bin:/opt/homebrew/sbin:/opt/local/bin:/opt/local/sbin:$DEF_PATH
      export MANPATH=/opt/local/share/man:$MANPATH
      ;;
    FreeBSD)
      export PATH=$DEF_PATH
      ;;
    Linux)
      export PATH=$DEF_PATH
      ;;
    *)
      export PATH=$DEF_PATH
      ;;
  esac
}
buildenv

# Docker env
export BUILDKIT_INLINE_CACHE=1
export DOCKER_BUILDKIT=1

# GO dev
export GOPATH=$HOME/work/go

# Default Lang Support
export LANG=en_US.UTF-8
export LC_ALL=C

# Editor
if [ "$(which emacs > /dev/null; echo $?)" = 0 ]; then
    export EDITOR="emacs -nw"
    export CSCOPE_EDITOR='ec'
    alias emacs="emacs -nw"
else
    export EDITOR="vi"
fi

# A better history
shopt -s histappend
export HISTCONTROL=ignoreboth # ignore space and dupes
export HISTFILESIZE=1000000
export HISTSIZE=1000000
export PROMPT_COMMAND='history -a'

# http://geoff.greer.fm/lscolors/
export CLICOLOR=1
export LSCOLORS="Cxfxcxdxbxegedabagacad"
export LS_COLORS="di=1;;40:ln=35;40:so=32;40:pi=33;40:ex=31;40:bd=34;46:cd=34;43:su=0;41:sg=0;46:tw=0;42:ow=0;43:"

function parse_git_branch {
  ref=$(git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/') || return
  echo "$ref"
}

# Sets the command prompt and xterm window title
function prompt
{
  case $TERM in
    xterm* | screen*)
      TITLEBAR="\[\033]0;\u@\h: \W\007\]"
      PROMPT="\[\033[1;32m\]\h:\[\033[0m\][\W]\[\033[0;32m\]\$(parse_git_branch)\[\033[0m\]$ "
      PS1="${TITLEBAR}${PROMPT}"
      ;;
    dumb)
      # emacs and friends
      TITLEBAR="\h:\W\$ "
      PROMPT="\h:\W\$ "
      PS1="${PROMPT}"
      ;;
    *)
      # everything else
      TITLEBAR="\h:\W\$ "
      PROMPT="\h:\W\$ "
      PS1="${TITLEBAR}${PROMPT}"
      ;;
  esac
}
prompt

# Helpful Aliases and Functions
alias lsd="ls -lta | grep dr | sort"
alias cdg='cd $(git root)'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias c='clear'
alias h="history 25"
alias j="jobs -l"
alias la="ls -a"
alias lf="ls -FA"
alias ll="ls -lA"
alias python=/opt/homebrew/bin/python3


if [ -S "$SSH_AUTH_SOCK" ] && [ ! -h "$SSH_AUTH_SOCK" ]; then
    ln -sf "$SSH_AUTH_SOCK" ~/.ssh/ssh_auth_sock
fi
export SSH_AUTH_SOCK=~/.ssh/ssh_auth_sock

# SSH Builder
ssh-herb() { ssh -A "herb@$*"; }
ssh-ubuntu() { ssh -A "ubuntu@$*"; }
ssh-ec2user() { ssh -A "ec2-user@$*"; }

# Useful functions
function bld()
{
    if which gmake >/dev/null; then
        MAKE="gmake"
    else
        MAKE="make"
    fi
    if [ -z "$CORES" ]; then
        cores=$(python3 -c "import multiprocessing; print(multiprocessing.cpu_count())")
    else
        cores=$CORES
    fi
    TIMEFORMAT="%R"
    build="$MAKE -j$cores $*"
    { TIME=$( { time $build 1>&3- 2>&4-; } 2>&1 ); } 3>&1 4>&2
    status=$?
    if [[ $status != 0 ]]; then
        echo "FAILED in $TIME seconds using $cores job(s) [ $* ] " >&2
    else
        echo "PASSED in $TIME seconds using $cores job(s) [ $* ] " >&2
    fi
    return $status
}

function aes_load()
{
    cmd=$(openssl aes-256-cbc -d -in "$1")
    eval "$cmd"
    unset cmd
}

function gpg_save()
{
    cmd=$(gpg --output "$1.gpg" --encrypt --recipient herb@brkt.com "$1")
    eval "$cmd"
    unset cmd
}

function gpg_load()
{
    cmd=$(gpg -o - "$1")
    eval "$cmd"
    unset cmd
}

function gpg_dump()
{
    gpg -o - "$1"
}

function cs()
{
    cat "$@" | cowsay -W80 -e @@ -T PP
}

function curlt()
{
    curl --styled-output -w "%{stderr}\
   namelookup:  %{time_namelookup}s\n\
      connect:  %{time_connect}s\n\
   appconnect:  %{time_appconnect}s\n\
  pretransfer:  %{time_pretransfer}s\n\
     redirect:  %{time_redirect}s\n\
starttransfer:  %{time_starttransfer}s\n\
-------------------------\n\
        total:  %{time_total}s\n" "$@"
}

# Bash extras
if [ -f ~/.bash.extra ]; then
    . ~/.bash.extra
fi
