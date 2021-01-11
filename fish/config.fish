set fish_greeting "
                       &    &     &
                        &&&&  &  && &
                &   &     && &&&&&&&
                 &&& &       &&&& &&&
              &&&&& &&& &   /~&&& &&   &&
             && &&&&_&_&_ & \&&&&&&&& && &&
                  &   &&\&&&&__&&&&&&&_/&&&  & &
                         \|\\\__/_/   &&& &
                         \_   _//     & &
                               \\
                                \\
                                //~
                                \\
                                /~
                                 /~
             ;,'             (---./~~\.---)
     _o_    ;:;'   ,          (          )
 ,-.'---`.__ ;      ~,         (________)
((j`~=~=~',-'     ,____.
 `-\     /        |    j
    `-=-'          `--'
"

set -l PRIVATE_FISH_CONFIGS_HOME $HOME/Dropbox/apps/fish

set -x XDG_CONFIG_HOME "$HOME/.config"
set -x XDG_DATA_HOME "$HOME/.local/share"
set -x PROJECTS_HOME "$HOME/Developer"

# install fisher
# TODO: move to Eru
set -g fisher_path $XDG_DATA_HOME/fisher
if not test -f $fisher_path/functions/fisher.fish
  curl https://git.io/fisher --create-dirs -sLo $fisher_path/functions/fisher.fish
  fish -c fisher
end
set fish_function_path $fish_function_path[1] $fisher_path/functions $fish_function_path[2..-1]
set fish_complete_path $fish_complete_path[1] $fisher_path/completions $fish_complete_path[2..-1]
for file in $fisher_path/conf.d/*.fish
    builtin source $file 2> /dev/null
end

function safe_source -a file
  if test -f $file
    source $file
  end
end

# private pre-configs
safe_source $PRIVATE_FISH_CONFIGS_HOME/preconfig.fish

# verify bin directories
if test ! -d $HOME/.local/bin
  mkdir -p $HOME/.local/bin
end
if test ! -d $XDG_CONFIG_HOME/bin
  mkdir -p $XDG_CONFIG_HOME/bin
end

# PATH
function __append_to_path -a path
  if test -d $path
    set -x PATH $path $PATH
  end
end
set -x PATH $XDG_CONFIG_HOME/bin $PATH
__append_to_path /opt/local/bin
__append_to_path $GEM_HOME/bin
__append_to_path /usr/texbin
__append_to_path /usr/local/sbin
__append_to_path /usr/local/opt/coreutils/libexec/gnubin
__append_to_path /usr/local/opt/gnu-sed/libexec/gnubin
__append_to_path /usr/local/opt/grep/bin
__append_to_path /usr/local/opt/gnu-getopt/bin
__append_to_path /usr/local/opt/texinfo/bin
__append_to_path $HOME/Dropbox/bin
__append_to_path $HOME/.doom-emacs/bin
set -x PATH $HOME/.local/bin $PATH

if command -v systemctl >/dev/null 2>&1
  systemctl --user import-environment PATH
end

# locale
set -x LANG en_GB.UTF-8
set -x LC_ALL en_GB.UTF-8

# variables
set -x EDITOR "emacsclient"
set cmd_notification_threshold 8000

# aliases
alias ghci "stack ghci"
alias ecl "emacsclient"
alias eclt "emacsclient -c"
alias cenv "cd $XDG_CONFIG_HOME"
alias cem  "cd $HOME/.emacs.d"

# TODO move to Eru
set -x BASE16_HOME $XDG_DATA_HOME/base16-shell
if test ! -d $BASE16_HOME
  git clone https://github.com/chriskempson/base16-shell.git $BASE16_HOME
end

# theme
if status --is-interactive
  if test "$TERM" = "linux"
    base16 onedark
  else
    base16 one-light
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
complete -c eru -a 'ssh repositories linking packages guardian os hardware' --no-files

# Vulpea
set -x VULPEA_DIR "$HOME/Dropbox/vulpea"
set -x BUDGET_DIR "$HOME/Dropbox/budget"
if command -v systemctl >/dev/null 2>&1
  systemctl --user import-environment VULPEA_DIR
end

# Emacs
set -x DOOMDIR "$XDG_CONFIG_HOME/emacs/doom"

# gpg + ssh
set -xg GPG_TTY (tty)
gpg-connect-agent updatestartuptty /bye >/dev/null

# nix
if test -f $HOME/.nix-profile/etc/profile.d/nix.sh
  bass source $HOME/.nix-profile/etc/profile.d/nix.sh
end

# nvm
set -x NVM_DIR "$XDG_CACHE_HOME/nvm"
set -x NVM_SOURCE "/usr/share/nvm"

# go
set -x GOPATH "$XDG_CACHE_HOME/go"
__append_to_path $GOPATH/bin

# gopass and friends
set -x PASSWORD_STORE "$HOME/Dropbox/.password-store"

# private post-configs
safe_source $PRIVATE_FISH_CONFIGS_HOME/postconfig.fish

# load bazel env
safe_source $HOME/.bazelenv

if status --is-interactive
  if test "$TERM" = "linux"
    clear
  end
end
