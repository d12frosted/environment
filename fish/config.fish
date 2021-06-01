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

set -x XDG_CONFIG_HOME "$HOME/.config"
set -x XDG_CACHE_HOME "$HOME/.cache"
set -x XDG_DATA_HOME "$HOME/.local/share"

function __append_to_path -a path
  if test -d $path
    set -g PATH $path $PATH
  end
end

set -g fisher_path $XDG_DATA_HOME/fisher
set fish_function_path $fish_function_path[1] $fisher_path/functions $fish_function_path[2..-1]
set fish_complete_path $fish_complete_path[1] $fisher_path/completions $fish_complete_path[2..-1]
for file in $fisher_path/conf.d/*.fish
    builtin source $file 2> /dev/null
end

################################################################################

status --is-login; and not set -q __fish_login_config_sourced
and begin
    # Login shell initialisation
    set -gx GPG_TTY (tty)
    if ! pgrep -x gpg-agent >/dev/null
        gpgconf --launch gpg-agent
    end

    # Make sure that we are loading this only once
    set -g __fish_login_config_sourced 1
end

################################################################################

status --is-interactive; and not set -q __fish_interactive_config_sourced
and begin
    __append_to_path /run/current-system/sw/bin
    __append_to_path /nix/var/nix/profiles/default/bin
    __append_to_path $HOME/.nix-profile/bin
    __append_to_path $HOME/.local/bin

    if test "$TERM" != linux
        base16 tomorrow
    end

    set -gx GPG_TTY (tty)
    if ! pgrep -x gpg-agent >/dev/null
        gpgconf --launch gpg-agent
    end

    # Make sure that we are loading this only once
    set -g __fish_interactive_config_sourced 1
end

################################################################################
