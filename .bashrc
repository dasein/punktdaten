##Default Paths
SPATH="/sbin:/usr/sbin:/usr/local/sbin"
if [ -z "$DEF_PATH" ]; then DEF_PATH=/usr/local/bin:$HOME/bin:$SPATH:$PATH; fi

##OS specific env builder
OS=`uname -a | awk '{print $1}'`
function buildenv
{
  case $OS in
    Darwin)
      export PATH=/opt/local/bin:/opt/local/sbin:$DEF_PATH
      export MANPATH=/opt/local/share/man:$MANPATH
      alias oncalendar="vrem 20"
      ;;
    FreeBSD)
      export PATH=$DEF_PATH
      ;;
    Linux)
      export PATH=$DEF_PATH:$HOME/work/tools/
      ;;
    *)
      export PATH=$DEF_PATH
      ;;
  esac
}
buildenv

##Default Lang Support
export LANG=en_US.UTF-8
export LC_ALL=C

##Editor
if [ `which emacs > /dev/null; echo $?` = 0 ]; then
    export EDITOR="emacs -nw"
    export CSCOPE_EDITOR='ec'
    alias emacs="emacs -nw"
else
    export EDITOR="vi"
fi

##Do not save dupes in the bash history file
export HISTCONTROL=ignoredups

##http://geoff.greer.fm/lscolors/
export CLICOLOR=1
export LSCOLORS="Cxfxcxdxbxegedabagacad"
export LS_COLORS="di=1;;40:ln=35;40:so=32;40:pi=33;40:ex=31;40:bd=34;46:cd=34;43:su=0;41:sg=0;46:tw=0;42:ow=0;43:"

##Sets the command prompt and xterm window title
function prompt
{
  case $TERM in
    xterm*)
      TITLEBAR='\[\033]0;\u@\h: \W\007\]'
      PROMPT="\[\033[1;32m\]\h:\[\033[0m\][\W]$ "
      PS1="${TITLEBAR}${PROMPT}"
      ;;
    dumb)
      # emacs and friends
      TITLEBAR='\h:\W\$ '
      PROMPT='\h:\W\$ '
      PS1="${PROMPT}"
      ;;
    *)
      # everything else
      TITLEBAR='\h:\W\$ '
      PROMPT='\h:\W\$ '
      PS1="${TITLEBAR}${PROMPT}"
      ;;
  esac
}
prompt

##Helpful Aliases and Functions
alias lsd="ls -lta | grep dr | sort"
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias c='clear'
alias h="history 25"
alias j="jobs -l"
alias la="ls -a"
alias lf="ls -FA"
alias ll="ls -lA"

##SSH Host Aliases
alias sshb="ssh -A -p 2772 hpfennig@billyjack"
alias sshu="ssh -A -p 2772 hpfennig@uub"
alias sshk="ssh -A -p 2772 hpfennig@kate"

##SSH Builder
vm() { ssh -A -X -Y -p 2772 hpfennig@$@; }

##SSH Forwarding
if [ -S "$SSH_AUTH_SOCK" ] && [ ! -h "$SSH_AUTH_SOCK" ]; then
    ln -sf "$SSH_AUTH_SOCK" ~/.ssh/ssh_auth_sock
fi
export SSH_AUTH_SOCK=~/.ssh/ssh_auth_sock

## Setup Virtualenv environment
export WORKON_HOME=$HOME/.virtualenvs
export PIP_VIRTUALENV_BASE=$WORKON_HOME
which virtualenvwrapper.sh > /dev/null 2>&1 && source `which virtualenvwrapper.sh`
