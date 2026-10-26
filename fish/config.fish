# macOS synthesizes LANG from Language + Region (en_UA.UTF-8), which is not
# a real locale, so set a valid one explicitly.
set -gx LANG en_US.UTF-8

# Homebrew
if test -x /opt/homebrew/bin/brew
    eval (/opt/homebrew/bin/brew shellenv fish)
end

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

# fnm (Coder images manage node with it)
if command -q fnm
    fnm env --use-on-cd --shell fish | source
end

# setup GPG as SSH agent, except on Coder, where SSH goes through what Coder
# sets up (its git SSH helper, or the agent forwarded by `coder ssh -A`)
set -gx GPG_TTY (tty)
if test "$CODER" != true
    set -gx SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
    if ! pgrep -x gpg-agent >/dev/null
        gpgconf --launch gpg-agent
    end
end

# start starship
if command -q starship
    starship init fish | source
end

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
if test -d "$HOME/.rd/bin"
    set --export --prepend PATH "$HOME/.rd/bin"
end
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)
