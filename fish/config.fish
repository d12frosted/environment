if status is-interactive
    # Commands to run in interactive sessions can go here
end

fish_add_path ~/.local/bin
fish_add_path ~/.config/bin

# activate mise (if installed)
if command -v mise &> /dev/null
    mise activate fish | source
end

# setup GPG as SSH agent
set -gx GPG_TTY (tty)
set -gx SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
if ! pgrep -x gpg-agent >/dev/null
    gpgconf --launch gpg-agent
end

# start starship
starship init fish | source
