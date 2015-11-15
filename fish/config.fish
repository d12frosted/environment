set -l PRIVATE_FISH_CONFIGS_HOME $HOME/Dropbox/Apps/fish

# private pre-configs
if test -f $PRIVATE_FISH_CONFIGS_HOME/preconfig.fish
  source $PRIVATE_FISH_CONFIGS_HOME/preconfig.fish
end

# variables
set -x XDG_CONFIG_HOME ~/.environment
set -x SPACEMACSDIR $XDG_CONFIG_HOME/emacs
set -x PATH $HOME/.local/bin /usr/texbin $PATH
set -x EDITOR "emacsclient"
set fish_greeting ""

# aliases
alias ghci "stack ghci"
alias eclient "emacsclient"
alias ec "emacsclient -nw"
alias cenv "cd $XDG_CONFIG_HOME"
alias cem  "cd $HOME/.emacs.d"

# theme
__d12_set_theme "tomorrow_night"

# python
eval (python -m virtualfish)

# private post-configs
if test -f $PRIVATE_FISH_CONFIGS_HOME/postconfig.fish
  source $PRIVATE_FISH_CONFIGS_HOME/postconfig.fish
end
