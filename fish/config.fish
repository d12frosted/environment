# variables

set -x PATH $HOME/.cabal/bin /usr/local/bin /bin /usr/sbin /sbin /usr/bin $HOME/.bin $HOME/.rvm/bin
set -x EDITOR "emacsclient"
set fish_greeting

# nix
eval (~/.environment/fish/nix_fish_env.sh) 2>/dev/null

# python
eval (python -m virtualfish)
