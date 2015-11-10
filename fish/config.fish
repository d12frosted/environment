# variables

set -x XDG_CONFIG_HOME ~/.environment
set -x SPACEMACSDIR $XDG_CONFIG_HOME/emacs
set -x PATH $HOME/.local/bin /usr/texbin $PATH
set -x EDITOR "emacsclient"
set fish_greeting ""

alias ghci "stack ghci"
alias e "emacsclient"
alias en "emacsclient -nw"

# theme
__set_theme "tomorrow_night"

# python
eval (python -m virtualfish)
