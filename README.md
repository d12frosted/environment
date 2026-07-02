<h1 align="center">d12frosted environment</h1>
<p align="center">
  <img width="256px" src="images/d12frosted.png" alt="Banner">
</p>
<p align="center">
  <a href="https://github.com/d12frosted/environment/actions/workflows/ci.yml">
    <img src="https://github.com/d12frosted/environment/actions/workflows/ci.yml/badge.svg" alt="CI Status Badge">
  </a>
  <a href="https://github.com/sponsors/d12frosted"><img alt="Sponsor" src="https://img.shields.io/badge/Sponsor-d12frosted-pink?logo=githubsponsors&logoColor=white"/></a>
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
./eru.sh install

# See what would happen without doing it
./eru.sh install --dry-run

# Run specific tasks
./eru.sh install packages wm

# Upgrade packages
./eru.sh upgrade packages
```

## Structure

```
~/.config/
├── eru.sh                   # Main bootstrap script
├── brew/                    # Homebrew packages
│   ├── Brewfile             # Common packages for all machines
│   ├── $USER.Brewfile       # User-specific packages (gitignored)
│   └── $HOSTNAME.Brewfile   # Machine-specific packages (gitignored)
├── macos/
│   └── defaults.sh          # macOS system preferences
├── launchd/                 # launchd service definitions (*.plist)
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
./eru.sh install homebrew
./eru.sh upgrade homebrew
```

### `packages`
Install packages from Brewfiles. Automatically processes:
- `brew/Brewfile` - Common packages
- `brew/$USER.Brewfile` - User-specific (e.g., `d12frosted.Brewfile`)
- `brew/$HOSTNAME.Brewfile` - Hostname-specific (e.g., `macbook-pro.Brewfile`)

```bash
./eru.sh install packages
./eru.sh upgrade packages
```

### `macos`
Configure macOS system defaults (Dock, Finder, keyboard, etc.).

```bash
./eru.sh install macos
```

### `shell`
Set up fish shell as the default shell.

```bash
./eru.sh install shell
```

### `wm`
Configure window manager (yabai + skhd):
- Sets up yabai sudoers for scripting additions
- Patches skhd LaunchAgent plist
- Restarts services

```bash
./eru.sh install wm
```

### `devtools`
Set up development tools:
- Generate SSH keys (if not present)
- Fix GPG permissions
- Configure git, etc.

```bash
./eru.sh install devtools
```

### `symlinks`
Create symlinks for configuration files that need to live outside `~/.config`.

Some programs (like GnuPG and SSH) don't support XDG Base Directory spec and expect configs in specific locations. This task symlinks configs from `~/.config/` to their expected locations:

- **GnuPG**: `~/.config/gnupg/*` → `~/.gnupg/*`
- **SSH**: `~/.config/ssh/config` → `~/.ssh/config`

Setup:

```bash
./eru.sh install symlinks
```

Features:
- Automatically backs up existing files before symlinking
- Skips files that are already correctly symlinked
- Fixes GnuPG permissions (600 for files, 700 for directories)

See [gnupg/README.md](gnupg/README.md) and [ssh/README.md](ssh/README.md) for details.

### `services`
Set up launchd services (macOS only). Symlinks every `launchd/*.plist` into
`~/Library/LaunchAgents`, creates any referenced log directories, and reloads
the services via `launchctl`.

```bash
./eru.sh install services
```

The vulpea note sync lives here. `bin/vulpea-sync` mirrors `~/vulpea` to git on
a 10-minute launchd backstop; `bin/vulpea-watch` (a KeepAlive `fswatch` daemon)
debounces file changes and kicks off a sync shortly after you stop editing, so a
burst of edits becomes a single push. Both funnel through `vulpea-sync`, which
takes a lock so runs never overlap.

### `emacs`
Set up Emacs configuration by running `emacs/setup.sh` (bootstraps packages and
generates autoloads). Supports subtasks via `emacs:<subtask>` syntax, passed
straight through to `setup.sh`: `install`, `upgrade`, `compile`, `lint`,
`test`, `clean`, `doctor`.

```bash
./eru.sh install emacs            # Full Emacs setup (install)
./eru.sh install emacs:compile    # Just byte-compile
./eru.sh install emacs:lint       # Just lint
./eru.sh install emacs:test       # Just run tests
```

## Usage

### Actions

- `install` - Install and configure
- `upgrade` - Update existing installations
- `doctor` - Run health checks without changing anything (see [Health Checks](#health-checks))

### Options

- `--dry-run` - Show what would be done without doing it
- `--force` - Skip dependency checks and continue anyway
- `--help` - Show help message

### Examples

```bash
# Full installation with dry-run first
./eru.sh install --dry-run
./eru.sh install

# Install just packages and window manager
./eru.sh install packages wm

# Upgrade packages only
./eru.sh upgrade packages

# Reset yabai/skhd configuration
./eru.sh install wm

# Run Emacs-specific subtask
./eru.sh install emacs:compile

# Force install even if dependencies missing
./eru.sh install wm --force

# Run all health checks
./eru.sh doctor

# Check only brew packages
./eru.sh doctor brew
```

## Health Checks

The `doctor` action runs read-only health checks. It never changes anything; it
just reports issues and exits non-zero if any are found. Run all checks, or pass
specific ones:

```bash
./eru.sh doctor              # Run all checks
./eru.sh doctor brew shell   # Run only the listed checks
```

Available checks:

- `brew` - Report installed packages that aren't tracked in any Brewfile (orphans)
- `shell` - Verify fish is the default shell
- `wm` - Check yabai and skhd are running
- `symlinks` - Verify managed symlinks point where they should
- `services` - Check launchd services are loaded
- `emacs` - Run `eldev lint` on the Emacs configuration

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
   ./eru.sh install --dry-run

   # Do it!
   ./eru.sh install
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
services (requires: packages, emacs → vulpea-sync, vulpea-watch)
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
./eru.sh install packages
```

## CI Testing

The repository includes GitHub Actions workflows that test:

- ✅ ShellCheck linting
- ✅ Dry-run mode on all tasks
- ✅ Homebrew installation
- ✅ Package installation
- ✅ Doctor health checks
- ✅ Help and error handling
- ✅ Script syntax validation

Run tests locally:

```bash
# Lint
shellcheck eru.sh macos/defaults.sh emacs/setup.sh

# Syntax check
bash -n eru.sh

# Dry run
./eru.sh install --dry-run

# Health checks
./eru.sh doctor
```

## Customization

### Adding a New Task

1. **Define the task function** in `eru.sh`:

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
./eru.sh install macos
```

## Troubleshooting

### Bootstrap is locked

```bash
rm ~/.cache/eru/eru.lock
```

### Dependency errors

Use `--force` to skip checks (use with caution):

```bash
./eru.sh install mytask --force
```

### Check what would happen

Always available:

```bash
./eru.sh install --dry-run
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

## Support

If you enjoy this project, you can support its development via [GitHub Sponsors](https://github.com/sponsors/d12frosted) or [Patreon](https://www.patreon.com/d12frosted).
