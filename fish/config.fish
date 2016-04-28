set -l PRIVATE_FISH_CONFIGS_HOME $HOME/Dropbox/Apps/fish

# private pre-configs
if test -f $PRIVATE_FISH_CONFIGS_HOME/preconfig.fish
  source $PRIVATE_FISH_CONFIGS_HOME/preconfig.fish
end

# variables
set -x XDG_CONFIG_HOME ~/.environment
set -x SPACEMACSDIR $XDG_CONFIG_HOME/emacs
set -x GEM_HOME $HOME/.local/gem
set -x GEM_PATH $HOME/.local/gem
set -x PATH $HOME/.local/bin $GEM_HOME/bin /usr/texbin $PATH
set -x EDITOR "emacsclient"
set fish_greeting ""

# aliases
alias ghci "stack ghci"
alias ecl "emacsclient"
alias eclt "emacsclient -nw"
alias emt "emacs -nw --insecure"
alias cenv "cd $XDG_CONFIG_HOME"
alias cem  "cd $HOME/.emacs.d"

function em
  emacs --insecure $argv &
end

# theme
set fish_color_autosuggestion gray
set fish_color_command purple
set fish_color_comment brown
set fish_color_cwd green
set fish_color_cwd_root red
set fish_color_error red
set fish_color_escape cyan
set fish_color_history_current cyan
set fish_color_match cyan
set fish_color_normal normal
set fish_color_operator cyan
set fish_color_param blue
set fish_color_quote green
set fish_color_redirection cyan
set fish_color_search_match \x2d\x2dbackground\x3dblack
set fish_color_selection \x2d\x2dbackground\x3dblack
set fish_color_valid_path \x2d\x2dunderline
set fish_pager_color_completion normal
set fish_pager_color_description yellow
set fish_pager_color_prefix cyan
set fish_pager_color_progress cyan

# python
eval (python -m virtualfish)

# private post-configs
if test -f $PRIVATE_FISH_CONFIGS_HOME/postconfig.fish
  source $PRIVATE_FISH_CONFIGS_HOME/postconfig.fish
end
