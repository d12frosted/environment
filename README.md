<h1 align="center">d12frosted environment</h1>
<p align="center">
  <img width="256px" src="images/d12frosted.png" alt="Banner">
</p>
<p align="center">
  <a href="https://github.com/d12frosted/environment/actions?query=workflow%3ACI">
    <img src="https://github.com/d12frosted/environment/workflows/CI/badge.svg" alt="CI Status Badge">
  </a>
  <a href="https://github.com/d12frosted/environment/actions?query=workflow%3AEmacs">
    <img src="https://github.com/d12frosted/environment/workflows/Emacs/badge.svg" alt="Emacs Status Badge">
  </a>
</p>

> *"Of the theme that I have declared to you, I will now that ye make in harmony together a Great Music."*
> — Eru Ilúvatar, The Silmarillion

A declarative dotfiles and system bootstrap for macOS (and potentially Linux).

Inspired by the Ainulindalë, where Eru Ilúvatar shaped the world through the harmony of the Great Music, this bootstrap system shapes your development environment with purpose and harmony.

## Philosophy

- **Pragmatic over idealistic**: Prefers practical solutions over theoretical perfection
- **Simple over complex**: Avoids unnecessary complexity and over-engineering
- **Maintainable**: Easy to understand, modify, and debug
- **Idempotent**: Safe to run multiple times
- **Selective**: Run everything or just specific tasks

## Quick Start

```bash
# Clone into ~/.config
git clone <your-repo> ~/.config
cd ~/.config

# Full installation
./bootstrap.sh install

# See what would happen without doing it
./bootstrap.sh install --dry-run

# Run specific tasks
./bootstrap.sh install packages wm

# Upgrade packages
./bootstrap.sh upgrade packages
```

## Structure

```
~/.config/
├── bootstrap.sh             # Main bootstrap script
├── brew/                    # Homebrew packages
│   ├── Brewfile             # Common packages for all machines
│   ├── $USER.Brewfile       # User-specific packages (gitignored)
│   └── $HOSTNAME.Brewfile   # Machine-specific packages (gitignored)
├── macos/
│   └── defaults.sh          # macOS system preferences
├── gnupg/                   # GnuPG configuration (symlinked to ~/.gnupg)
│   ├── ...
│   └── README.md
├── ssh/                     # SSH configuration (symlinked to ~/.ssh)
│   ├── ...
│   └── README.md
├── fish/                    # Fish shell configuration
├── git/                     # Git configuration
├── alacritty/               # Alacritty configuration
├── ...
└── emacs/                   # Emacs configuration
```

## Tasks

The bootstrap system is organized into independent tasks:

### `homebrew`
Install or update Homebrew itself.

```bash
./bootstrap.sh install homebrew
./bootstrap.sh upgrade homebrew
```

### `packages`
Install packages from Brewfiles. Automatically processes:
- `brew/Brewfile` - Common packages
- `brew/$USER.Brewfile` - User-specific (e.g., `d12frosted.Brewfile`)
- `brew/$HOSTNAME.Brewfile` - Hostname-specific (e.g., `macbook-pro.Brewfile`)

```bash
./bootstrap.sh install packages
./bootstrap.sh upgrade packages
```

### `macos`
Configure macOS system defaults (Dock, Finder, keyboard, etc.).

```bash
./bootstrap.sh install macos
```

### `shell`
Set up fish shell as the default shell.

```bash
./bootstrap.sh install shell
```

### `wm`
Configure window manager (yabai + skhd):
- Sets up yabai sudoers for scripting additions
- Patches skhd LaunchAgent plist
- Restarts services

```bash
./bootstrap.sh install wm
```

### `devtools`
Set up development tools:
- Generate SSH keys (if not present)
- Fix GPG permissions
- Configure git, etc.

```bash
./bootstrap.sh install devtools
```

### `symlinks`
Create symlinks for configuration files that need to live outside `~/.config`.

Some programs (like GnuPG and SSH) don't support XDG Base Directory spec and expect configs in specific locations. This task symlinks configs from `~/.config/` to their expected locations:

- **GnuPG**: `~/.config/gnupg/*` → `~/.gnupg/*`
- **SSH**: `~/.config/ssh/config` → `~/.ssh/config`

Setup:

```bash
./bootstrap.sh install symlinks
```

Features:
- Automatically backs up existing files before symlinking
- Skips files that are already correctly symlinked
- Fixes GnuPG permissions (600 for files, 700 for directories)

See [gnupg/README.md](gnupg/README.md) and [ssh/README.md](ssh/README.md) for details.

### `emacs`
Set up Emacs configuration. Supports subtasks:

```bash
./bootstrap.sh install emacs           # All Emacs setup
./bootstrap.sh install emacs:config    # Just config
./bootstrap.sh install emacs:db        # Just database sync
```

## Usage

### Actions

- `install` - Install and configure
- `upgrade` - Update existing installations

### Options

- `--dry-run` - Show what would be done without doing it
- `--force` - Skip dependency checks and continue anyway
- `--help` - Show help message

### Examples

```bash
# Full installation with dry-run first
./bootstrap.sh install --dry-run
./bootstrap.sh install

# Install just packages and window manager
./bootstrap.sh install packages wm

# Upgrade packages only
./bootstrap.sh upgrade packages

# Reset yabai/skhd configuration
./bootstrap.sh install wm

# Run Emacs-specific subtask
./bootstrap.sh install emacs:config

# Force install even if dependencies missing
./bootstrap.sh install wm --force
```

## Setting Up Your Machine

### First Time Setup

1. **Clone the repository**
   ```bash
   git clone <your-repo> ~/.config
   cd ~/.config
   ```

2. **Create your Brewfiles**
   ```bash
   # Copy and customize user-specific packages
   cp brew/example-user.Brewfile brew/$USER.Brewfile

   # Optional: hostname-specific packages
   cp brew/example-hostname.Brewfile brew/$(hostname -s).Brewfile
   ```

3. **Edit configurations**
   - Update `brew/Brewfile` with common packages
   - Customize `macos/defaults.sh` to your preferences
   - Add your application configs (fish, git, etc.)

4. **Run bootstrap**
   ```bash
   # See what will happen
   ./bootstrap.sh install --dry-run

   # Do it!
   ./bootstrap.sh install
   ```

### Dependency Chain

Tasks have implicit dependencies. The bootstrap script performs basic checks:

```
homebrew (standalone)
  ↓
packages (requires: homebrew)
  ↓
wm (requires: packages → yabai, skhd)
shell (requires: packages → fish)
devtools (requires: packages → gnupg, ssh)
emacs (requires: packages → emacs-plus)
```

When running individual tasks, you'll get helpful error messages if dependencies are missing.

## Adding New Packages

### Common Packages (All Machines)

Edit `brew/Brewfile`:

```ruby
brew "your-package"
cask "your-app"
```

### User-Specific Packages

Edit `brew/$USER.Brewfile`:

```ruby
# Personal tools
brew "your-personal-tool"
```

### Machine-Specific Packages

Edit `brew/$HOSTNAME.Brewfile`:

```ruby
# Work laptop only
brew "kubectl"
cask "zoom"
```

Then run:

```bash
./bootstrap.sh install packages
```

## CI Testing

The repository includes GitHub Actions workflows that test:

- ✅ ShellCheck linting
- ✅ Dry-run mode on all tasks
- ✅ Homebrew installation
- ✅ Package installation
- ✅ Help and error handling
- ✅ Script syntax validation

Run tests locally:

```bash
# Lint
shellcheck bootstrap.sh macos/defaults.sh

# Syntax check
bash -n bootstrap.sh

# Dry run
./bootstrap.sh install --dry-run
```

## Customization

### Adding a New Task

1. **Define the task function** in `bootstrap.sh`:

```bash
function task_mytask() {
  task_start "mytask" "Setting up my task"

  # Your logic here

  task_complete "mytask" "My task configured"
}
```

2. **Register the task**:

```bash
declare -A TASKS=(
  ...
  [mytask]="task_mytask"
)
```

3. **Add to default tasks** (optional):

```bash
DEFAULT_TASKS=(... mytask)
```

### Modifying macOS Defaults

Edit `macos/defaults.sh` and add your `defaults write` commands:

```bash
# Example: Disable animations
defaults write NSGlobalDomain NSAutomaticWindowAnimationsEnabled -bool false
```

Run:

```bash
./bootstrap.sh install macos
```

## Troubleshooting

### Bootstrap is locked

```bash
rm ~/.cache/bootstrap/bootstrap.lock
```

### Dependency errors

Use `--force` to skip checks (use with caution):

```bash
./bootstrap.sh install mytask --force
```

### Check what would happen

Always available:

```bash
./bootstrap.sh install --dry-run
```

### Homebrew not in PATH

The script automatically adds Homebrew to PATH, but if you're getting "brew not found":

```bash
# Apple Silicon
eval "$(/opt/homebrew/bin/brew shellenv)"

# Intel
eval "$(/usr/local/bin/brew shellenv)"
```

## Why Not Nix?

This setup deliberately avoids Nix/nix-darwin/home-manager because:

- macOS is not designed for Nix
- Emacs is not designed for Nix
- Complexity cost > reproducibility benefits (for this use case)
- Homebrew is simpler and "just works" on macOS

If you want Nix, that's cool! But this repo is explicitly for a simpler approach.

## License

Use it however you want. No warranty. Your configs, your responsibility.
