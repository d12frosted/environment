set -l PRIVATE_FISH_CONFIGS_HOME $HOME/Dropbox/Apps/fish

# private pre-configs
if test -f $PRIVATE_FISH_CONFIGS_HOME/preconfig.fish
  source $PRIVATE_FISH_CONFIGS_HOME/preconfig.fish
end

# verify bin directories
if test ! -d $HOME/.local/bin
  mkdir -p $HOME/.local/bin
end
if test ! -d $XDG_CONFIG_HOME/utils/bin
  mkdir -p $XDG_CONFIG_HOME/utils/bin
end

# variables
set -x GPG_TTY /dev/pts/0
set -x SPACEMACSDIR $HOME/.spacemacs
set -x GEM_HOME $HOME/.local/gem
set -x GEM_PATH $HOME/.local/gem
set -x EDITOR "emacsclient"
set fish_greeting ""
set cmd_notification_threshold 8000

# PATH
function __append_to_path -a path
  if test -d $path
    set -x PATH $path $PATH
  end
end
set -x PATH $HOME/.local/bin $PATH
set -x PATH $XDG_CONFIG_HOME/utils/bin $PATH
__append_to_path $GEM_HOME/bin
__append_to_path /usr/texbin
__append_to_path /usr/local/sbin

# aliases
alias ghci "stack ghci"
alias ecl "emacsclient"
alias eclt "emacsclient -c"
alias cenv "cd $XDG_CONFIG_HOME"
alias cem  "cd $HOME/.emacs.d"

# theme
if test "$TERM" = "linux"
  set FISH_THEME base16-tomorrow-night
else
  set FISH_THEME base16-tomorrow
end

if status --is-interactive
  # load current theme
  eval sh "$XDG_CONFIG_HOME/fish/themes/$FISH_THEME.sh"

  function theme_preview_mode
    for SCRIPT in $XDG_CONFIG_HOME/fish/themes/*.sh
      set THEME (basename $SCRIPT .sh)
      set FUNCTION_NAME (echo "set_theme_$THEME")
      function $FUNCTION_NAME -V SCRIPT -V THEME
        eval sh '"'$SCRIPT'"'
      end
    end for
  end
end
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

# Eru completions
complete -c eru -a 'ssh repositories linking packages guardian os' --no-files

# private post-configs
if test -f $PRIVATE_FISH_CONFIGS_HOME/postconfig.fish
  source $PRIVATE_FISH_CONFIGS_HOME/postconfig.fish
end

# ssh-agent
# https://github.com/danhper/fish-ssh-agent
if test -z "$SSH_ENV"
    set -xg SSH_ENV $HOME/.ssh/environment
end

if not __ssh_agent_is_started
    __ssh_agent_start
end

set fish_greeting "
             ;,'
     _o_    ;:;'
 ,-.'---`.__ ;       ,
((j`~=~=~',-'        ~,
 `-\     /        \˘˘˘˘/
    `-=-'          `ˉˉˇ
"
