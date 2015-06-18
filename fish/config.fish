# variables

set -x PATH $HOME/.cabal/bin /usr/local/bin /bin /usr/sbin /sbin /usr/bin $HOME/.bin $HOME/.rvm/bin
set -x EDITOR "emacsclient"
set fish_greeting

# theme
set fish_color_autosuggestion "8e908c"
set fish_color_command "8959a8"
set fish_color_comment "eab700"
set fish_color_end "8959a8"
set fish_color_error "c82829"
set fish_color_param "4271ae"
set fish_color_quote "718c00"
set fish_color_redirection "3e999f"

# nix
eval (~/.environment/fish/nix_fish_env.sh) 2>/dev/null

# python
eval (python -m virtualfish)
