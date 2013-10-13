# .bash_profile sourced in login shells.
if [ -f .bashrc ]; then
    . .bashrc
fi

# bash aliases
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi
