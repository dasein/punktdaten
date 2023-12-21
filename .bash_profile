# .bash_profile sourced in login shells.
if [ -f .bashrc ]; then
    . .bashrc
fi

# bash aliases
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# The next line updates PATH for the Google Cloud SDK.
if [ -f /Users/herbert/Downloads/google-cloud-sdk/path.bash.inc ]; then
  source '/Users/herbert/Downloads/google-cloud-sdk/path.bash.inc'
fi
eval "$(/opt/homebrew/bin/brew shellenv)"
