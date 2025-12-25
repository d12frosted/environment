# Homebrew
eval (/opt/homebrew/bin/brew shellenv)

if status is-interactive
    # Commands to run in interactive sessions can go here
end

fish_add_path ~/.local/bin
fish_add_path ~/.config/bin

# Go XDG paths
set -gx GOPATH ~/.local/share/go
set -gx GOMODCACHE ~/.cache/go/mod
set -gx GOBIN ~/.local/bin

# Bun
set -gx BUN_INSTALL ~/.bun
fish_add_path $BUN_INSTALL/bin

# setup GPG as SSH agent
set -gx GPG_TTY (tty)
set -gx SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
if ! pgrep -x gpg-agent >/dev/null
    gpgconf --launch gpg-agent
end

# start starship
starship init fish | source
