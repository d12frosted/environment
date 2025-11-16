#!/usr/bin/env bash
#
################################################################################
#
# Of the theme that I have declared to you, I will now that ye make in harmony
# together a Great Music. And since I have kindled you with the Flame
# Imperishable, ye shall show forth your powers in adorning this theme, each
# with his own thoughts and devices, if he will. But I will sit and hearken,
# and be glad that through you great beauty has been wakened into song.
#
#   — Eru Ilúvatar, The Silmarillion
#
################################################################################
#
# Bootstrap script for ~/.config
#
# A declarative system for bootstrapping macOS (and potentially Linux) with:
# - Homebrew package management
# - macOS defaults configuration
# - Shell setup (fish)
# - Window manager configuration (yabai + skhd)
# - Development tools (git, ssh, gpg)
# - Emacs configuration
#
################################################################################

set -euo pipefail

#
# OS Detection
#

KERNEL_NAME=$(uname -s | tr '[:upper:]' '[:lower:]')
KERNEL_RELEASE=$(uname -r | tr '[:upper:]' '[:lower:]')
OS_NAME="unknown"
OS_VERSION="unknown"

case $KERNEL_NAME in
  darwin)
    OS_NAME=macos
    OS_VERSION=$(sw_vers -productVersion)
    ;;
  linux)
    case $KERNEL_RELEASE in
      *arch*|*coreos*)
        OS_NAME="arch"
        ;;
    esac
    ;;
  *)
    ;;
esac

#
# User & Paths
#

if [ -z "${USER:-}" ]; then
  USER=$(whoami)
fi

export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"

SCRIPT_DIR="$(cd "$(dirname "$(readlink "${BASH_SOURCE[0]}")")" && pwd)"
mkdir -p "$HOME/.local/bin"

#
# Logging Functions
#

RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
GRAY='\033[0;37m'
RESET='\033[0m'

function log() {
  echo -e "$*"
}

function error() {
  echo -e "${RED}✗ $*${RESET}" >&2
}

function success() {
  echo -e "${GREEN}✓ $*${RESET}"
}

function info() {
  echo -e "${BLUE}→ $*${RESET}"
}

function warn() {
  echo -e "${YELLOW}⚠ $*${RESET}"
}

function task_start() {
  local task=$1
  local description=${2:-$task}
  if [[ "$DRY_RUN" == "true" ]]; then
    log "${BLUE}[DRY RUN] $description${RESET}"
  else
    log "${BLUE}▶ $description${RESET}"
  fi
}

function task_complete() {
  local task=$1
  local description=${2:-$task}
  if [[ "$DRY_RUN" != "true" ]]; then
    success "$description"
  fi
}

function task_skip() {
  local reason=$1
  log "${GRAY}⊘ Skipped: $reason${RESET}"
}

function show_greeting() {
  cat << 'EOF'

            ╭────────────────────────────────────────────────╮
            │                                                │
            │        ⟡  The Ainulindalë Begins  ⟡            │
            │                                                │
            │     "In the beginning Eru, the One, who in     │
            │      the Elvish tongue is named Ilúvatar,      │
            │       made the Ainur of his thought..."        │
            │                                                │
            ╰────────────────────────────────────────────────╯

                        ✦       ✦       ✦
                    ✦       ⟡       ⟡       ✦
                        ⟡   Theme of   ⟡
                    ✦       ⟡       ⟡       ✦
                        ✦       ✦       ✦

          Shaping your environment with harmony and purpose...

EOF
}

function show_farewell() {
  cat << 'EOF'

                        ✦       ✦       ✦
                              ⟡   ⟡
                    "And great beauty has been
                       wakened into song."
                              ⟡   ⟡
                        ✦       ✦       ✦

EOF
}

#
# Helper Functions
#

function fail() {
  error "$1"
  exit 1
}

function check_command() {
  local cmd=$1
  local error_msg=${2:-"$cmd is not installed"}

  if ! command -v "$cmd" &> /dev/null; then
    if [[ "${DRY_RUN:-false}" == "true" ]]; then
      warn "$error_msg (skipping check in dry-run mode)"
      return 1
    elif [[ "${FORCE:-false}" == "true" ]]; then
      warn "$error_msg (continuing due to --force)"
      return 1
    else
      fail "$error_msg"
    fi
  fi
  return 0
}

function is_macos() {
  [[ "$OS_NAME" == "macos" ]]
}

function is_linux() {
  [[ "$KERNEL_NAME" == "linux" ]]
}

function check_homebrew() {
  command -v brew &> /dev/null
}

#
# Lock File Management
#

LOCK_FILE="$XDG_CACHE_HOME/bootstrap/bootstrap.lock"

function acquire_lock() {
  if [ -f "$LOCK_FILE" ]; then
    fail "Eru is already running. If this is an error, delete $LOCK_FILE"
  fi

  mkdir -p "$(dirname "$LOCK_FILE")"
  touch "$LOCK_FILE"
}

function release_lock() {
  rm -f "$LOCK_FILE"
}

trap release_lock INT TERM EXIT

#
# Task: Homebrew
#

function task_homebrew() {
  task_start "homebrew" "Setting up Homebrew"

  if ! is_macos; then
    task_skip "Not on macOS"
    return 0
  fi

  if check_homebrew; then
    if [[ "$ACTION" == "upgrade" ]]; then
      info "Updating Homebrew..."
      if [[ "$DRY_RUN" != "true" ]]; then
        if ! brew update; then
          fail "Failed to update Homebrew"
        fi
      fi
    else
      info "Homebrew already installed at $(command -v brew)"
    fi
  else
    info "Installing Homebrew..."
    if [[ "$DRY_RUN" != "true" ]]; then
      if ! /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"; then
        fail "Failed to install Homebrew"
      fi

      # Add to PATH for this session
      if [[ -f "/opt/homebrew/bin/brew" ]]; then
        eval "$(/opt/homebrew/bin/brew shellenv)"
      fi
    fi
  fi

  task_complete "homebrew" "Homebrew ready"
}

#
# Task: Packages
#

function task_packages() {
  task_start "packages" "Installing packages"

  if ! is_macos; then
    task_skip "Not on macOS (Homebrew only)"
    return 0
  fi

  if ! check_homebrew; then
    if [[ "${DRY_RUN:-false}" == "true" ]]; then
      warn "Homebrew not installed (skipping check in dry-run mode)"
    else
      fail "Homebrew not installed. Run: $0 install homebrew"
    fi
  fi

  local brewfiles=()

  # Common Brewfile
  if [[ -f "$SCRIPT_DIR/brew/Brewfile" ]]; then
    brewfiles+=("$SCRIPT_DIR/brew/Brewfile")
  fi

  # User-specific Brewfile
  if [[ -f "$SCRIPT_DIR/brew/$USER.Brewfile" ]]; then
    brewfiles+=("$SCRIPT_DIR/brew/$USER.Brewfile")
  fi

  # Hostname-specific Brewfile
  local hostname
  hostname=$(hostname -s)
  if [[ -f "$SCRIPT_DIR/brew/$hostname.Brewfile" ]]; then
    brewfiles+=("$SCRIPT_DIR/brew/$hostname.Brewfile")
  fi

  if [[ ${#brewfiles[@]} -eq 0 ]]; then
    warn "No Brewfiles found in $SCRIPT_DIR/brew/"
    return 0
  fi

  for brewfile in "${brewfiles[@]}"; do
    info "Processing $(basename "$brewfile")..."
    if [[ "$DRY_RUN" != "true" ]]; then
      local bundle_args=("--file=$brewfile")
      [[ -n "${FORCE:-}" ]] && bundle_args+=("--no-upgrade")
      [[ "${FORMULA_ONLY:-false}" == "true" ]] && bundle_args+=("--formula")

      if ! brew bundle "${bundle_args[@]}"; then
        fail "Failed to install packages from $(basename "$brewfile")"
      fi
    fi
  done

  # claude-code
  if command -v claude &> /dev/null; then
    info "Claude is already installed"
  else
    info "Installing Claude"
    if [[ "$DRY_RUN" != "true" ]]; then
      curl -fsSL https://claude.ai/install.sh | bash
    fi
  fi

  task_complete "packages" "Packages installed"
}

#
# Task: macOS Defaults
#

function task_macos() {
  task_start "macos" "Configuring macOS defaults"

  if ! is_macos; then
    task_skip "Not on macOS"
    return 0
  fi

  local defaults_script="$XDG_CONFIG_HOME/macos/defaults.sh"

  if [[ ! -f "$defaults_script" ]]; then
    warn "No defaults script found at $defaults_script"
    return 0
  fi

  info "Running macOS defaults configuration..."
  if [[ "$DRY_RUN" != "true" ]]; then
    if ! bash "$defaults_script"; then
      fail "Failed to apply macOS defaults"
    fi
  fi

  task_complete "macos" "macOS defaults configured"
}

#
# Task: Shell Setup
#

function task_shell() {
  task_start "shell" "Setting up fish shell"

  check_command fish "fish not installed. Run: $0 install packages"

  local fish_path
  fish_path=$(command -v fish)

  # Check if fish is the default shell
  if [[ "$SHELL" != "$fish_path" ]]; then
    info "Setting fish as default shell..."

    # Ensure fish is in /etc/shells
    if ! grep -q "$fish_path" /etc/shells; then
      if [[ "$DRY_RUN" != "true" ]]; then
        if ! echo "$fish_path" | sudo tee -a /etc/shells > /dev/null; then
          fail "Failed to add fish to /etc/shells"
        fi
      fi
    fi

    # Change default shell
    if [[ "$DRY_RUN" != "true" ]]; then
      if ! sudo chsh -s "$fish_path" "$USER"; then
        fail "Failed to change default shell to fish"
      fi
      success "Default shell changed to fish (restart terminal to take effect)"
    fi
  else
    info "Fish is already the default shell"
  fi

  # sync and apply tinty
  tinty sync
  tinty apply base16-chinoiserie

  task_complete "shell" "Shell configured"
}

#
# Task: Window Manager (yabai + skhd)
#

function task_wm() {
  task_start "wm" "Configuring window manager"

  if ! is_macos; then
    task_skip "Not on macOS"
    return 0
  fi

  check_command yabai "yabai not installed. Run: $0 install packages"
  check_command skhd "skhd not installed. Run: $0 install packages"

  # Configure yabai sudoers
  info "Configuring yabai sudoers..."
  local yabai_path
  yabai_path=$(command -v yabai)
  local yabai_hash
  yabai_hash=$(shasum -a 256 "$yabai_path" | cut -d " " -f 1)

  if [[ "$DRY_RUN" != "true" ]]; then
    if ! echo "$(whoami) ALL=(root) NOPASSWD: sha256:$yabai_hash $yabai_path --load-sa" | \
      sudo tee /private/etc/sudoers.d/yabai > /dev/null; then
      fail "Failed to configure yabai sudoers"
    fi

    info "Restarting yabai..."
    yabai --stop-service || true
    if ! yabai --start-service; then
      fail "Failed to start yabai service"
    fi
  fi

  # Configure skhd
  info "Configuring skhd..."
  local plist_path="$HOME/Library/LaunchAgents/com.koekeishiya.skhd.plist"

  if [[ -f "$plist_path" ]] && grep -q '<key>SHELL</key>' "$plist_path"; then
    info "skhd already configured"
    if [[ "$DRY_RUN" != "true" ]]; then
      if ! skhd --restart-service; then
        fail "Failed to restart skhd service"
      fi
    fi
  else
    info "Patching skhd plist..."
    if [[ "$DRY_RUN" != "true" ]]; then
      skhd --stop-service || true
      skhd --uninstall-service || true
      if ! skhd --install-service; then
        fail "Failed to install skhd service"
      fi
      if ! /usr/libexec/PlistBuddy -c 'add :EnvironmentVariables:SHELL string /bin/sh' "$plist_path"; then
        fail "Failed to patch skhd plist"
      fi
      if ! skhd --start-service; then
        fail "Failed to start skhd service"
      fi
    fi
  fi

  info "Patching skhd PATH"
  if [[ "$DRY_RUN" != "true" ]]; then
    skhd --stop-service || true
    if ! /usr/libexec/PlistBuddy -c "add :EnvironmentVariables:PATH string $PATH" "$plist_path" 2>/dev/null; then
      if ! /usr/libexec/PlistBuddy -c "Set :EnvironmentVariables:PATH $PATH" "$plist_path"; then
        fail "Failed to patch skhd plist"
      fi
    fi
    if ! skhd --start-service; then
        fail "Failed to start skhd service"
      fi
  fi

  task_complete "wm" "Window manager configured"
}

#
# Task: Development Tools
#

function task_devtools() {
  task_start "devtools" "Setting up development tools"

  # SSH key generation
  if [[ ! -f "$HOME/.ssh/id_ed25519" ]]; then
    info "Generating SSH key..."
    if [[ "$DRY_RUN" != "true" ]]; then
      mkdir -p "$HOME/.ssh"
      if ! ssh-keygen -t ed25519 -C "${USER}@$(hostname)" -f "$HOME/.ssh/id_ed25519" -N ""; then
        fail "Failed to generate SSH key"
      fi
      success "SSH key generated at $HOME/.ssh/id_ed25519"
      warn "Add your SSH public key to GitHub/GitLab:"
      cat "$HOME/.ssh/id_ed25519.pub"
    fi
  else
    info "SSH key already exists"
  fi

  # GPG permissions fix
  if [[ -d "$HOME/.gnupg" ]]; then
    info "Fixing GPG permissions..."
    if [[ "$DRY_RUN" != "true" ]]; then
      if ! chown -R "$(whoami)" "$HOME/.gnupg/"; then
        warn "Failed to change ownership of GPG directory"
      fi
      if ! find "$HOME/.gnupg" -type f -exec chmod 600 {} \;; then
        warn "Failed to fix GPG file permissions"
      fi
      if ! find "$HOME/.gnupg" -type d -exec chmod 700 {} \;; then
        warn "Failed to fix GPG directory permissions"
      fi
    fi
  fi

  task_complete "devtools" "Development tools configured"
}

#
# Task: Symlinks
#

function task_symlinks() {
  task_start "symlinks" "Creating symlinks"

  # Helper function to create a symlink with backup
  create_symlink() {
    local source=$1
    local target=$2

    # Check if source exists
    if [[ ! -e "$source" ]]; then
      info "Skipping $target (source not found: $source)"
      return 0
    fi

    # Create target directory if needed
    local target_dir
    target_dir=$(dirname "$target")
    if [[ ! -d "$target_dir" ]]; then
      info "Creating directory: $target_dir"
      if [[ "$DRY_RUN" != "true" ]]; then
        mkdir -p "$target_dir"
      fi
    fi

    # Handle existing target
    if [[ -e "$target" || -L "$target" ]]; then
      # Check if it's already the correct symlink
      if [[ -L "$target" ]] && [[ "$(readlink "$target")" == "$source" ]]; then
        info "Symlink already exists: $target → $source"
        return 0
      fi

      # Backup existing file/symlink
      local backup
      backup="${target}.backup.$(date +%Y%m%d_%H%M%S)"
      warn "Backing up existing $target to $backup"
      if [[ "$DRY_RUN" != "true" ]]; then
        mv "$target" "$backup"
      fi
    fi

    # Create symlink
    info "Creating symlink: $target → $source"
    if [[ "$DRY_RUN" != "true" ]]; then
      if ln -s "$source" "$target"; then
        success "Created symlink: $target"
      else
        warn "Failed to create symlink: $target"
        return 1
      fi
    fi
  }

  # GnuPG configuration symlinks
  # GnuPG doesn't support XDG_CONFIG_HOME, so we symlink from ~/.config/gnupg to ~/.gnupg
  if [[ -d "$XDG_CONFIG_HOME/gnupg" ]]; then
    info "Setting up GnuPG symlinks..."

    # Symlink all files from ~/.config/gnupg to ~/.gnupg
    # Skip .example files and README.md - only symlink actual configs
    for config_file in "$XDG_CONFIG_HOME/gnupg"/*; do
      if [[ -f "$config_file" ]]; then
        local filename
        filename=$(basename "$config_file")

        # Skip example files and READMEs
        if [[ "$filename" == *.example ]] || [[ "$filename" == "README.md" ]]; then
          continue
        fi

        create_symlink "$config_file" "$HOME/.gnupg/$filename"
      fi
    done

    # Fix GnuPG permissions after creating symlinks
    if [[ -d "$HOME/.gnupg" ]] && [[ "$DRY_RUN" != "true" ]]; then
      info "Fixing GnuPG permissions..."
      if ! chown -R "$(whoami)" "$HOME/.gnupg/" 2>/dev/null; then
        warn "Failed to change ownership of GPG directory (may be normal)"
      fi
      if ! find "$HOME/.gnupg" -type f -exec chmod 600 {} \; 2>/dev/null; then
        warn "Failed to fix GPG file permissions"
      fi
      if ! find "$HOME/.gnupg" -type d -exec chmod 700 {} \; 2>/dev/null; then
        warn "Failed to fix GPG directory permissions"
      fi
    fi
  else
    info "No GnuPG config found at $XDG_CONFIG_HOME/gnupg"
  fi

  # SSH configuration symlinks
  # SSH can use ~/.ssh but we keep configs in ~/.config/ssh for consistency
  if [[ -d "$XDG_CONFIG_HOME/ssh" ]]; then
    info "Setting up SSH symlinks..."

    # Symlink SSH config files (skip .example files and READMEs)
    for ssh_file in "$XDG_CONFIG_HOME/ssh"/*; do
      if [[ -f "$ssh_file" ]]; then
        local filename
        filename=$(basename "$ssh_file")

        # Skip example files and READMEs
        if [[ "$filename" == *.example ]] || [[ "$filename" == "README.md" ]]; then
          continue
        fi

        create_symlink "$ssh_file" "$HOME/.ssh/$filename"
      fi
    done

    # Fix SSH file permissions (SSH requires strict permissions)
    if [[ "$DRY_RUN" != "true" ]]; then
      info "Fixing SSH permissions..."
      # Config file must be 600 (rw-------)
      if [[ -f "$XDG_CONFIG_HOME/ssh/config" ]]; then
        chmod 600 "$XDG_CONFIG_HOME/ssh/config" 2>/dev/null || true
      fi
      # Private keys must be 600
      find "$XDG_CONFIG_HOME/ssh" -type f -name "id_*" ! -name "*.pub" -exec chmod 600 {} \; 2>/dev/null || true
      # Public keys can be 644 (rw-r--r--)
      find "$XDG_CONFIG_HOME/ssh" -type f -name "*.pub" -exec chmod 644 {} \; 2>/dev/null || true
    fi
  else
    info "No SSH config found at $XDG_CONFIG_HOME/ssh"
  fi

  # symlink bootstrap.sh to eru
  create_symlink "$XDG_CONFIG_HOME/bootstrap.sh" "$HOME/.local/bin/eru"

  task_complete "symlinks" "Symlinks created"
}

#
# Task: Emacs
#

function task_emacs() {
  task_start "emacs" "Setting up Emacs"

  check_command emacs "Emacs not installed. Run: $0 install packages"

  local emacs_dir="$XDG_CONFIG_HOME/emacs"

  if [[ -f "$emacs_dir/setup.sh" ]]; then
    info "Running Emacs setup script..."
    if [[ "$DRY_RUN" != "true" ]]; then
      cd "$emacs_dir"
      # Pass subtasks if any were specified, otherwise run default (install)
      if [[ ${#EMACS_SUBTASKS[@]} -gt 0 ]]; then
        if ! bash setup.sh "${EMACS_SUBTASKS[@]}"; then
          fail "Failed to run Emacs setup script"
        fi
      else
        if ! bash setup.sh; then
          fail "Failed to run Emacs setup script"
        fi
      fi
    fi
  else
    warn "No Emacs setup script found at $emacs_dir/setup.sh"
  fi

  task_complete "emacs" "Emacs configured"
}

#
# Task Registry
#

# Check if a task name is valid
function is_valid_task() {
  case "$1" in
    homebrew|packages|macos|shell|wm|devtools|symlinks|emacs)
      return 0
      ;;
    *)
      return 1
      ;;
  esac
}

# Get the function name for a task
function get_task_function() {
  case "$1" in
    homebrew) echo "task_homebrew" ;;
    packages) echo "task_packages" ;;
    macos) echo "task_macos" ;;
    shell) echo "task_shell" ;;
    wm) echo "task_wm" ;;
    devtools) echo "task_devtools" ;;
    symlinks) echo "task_symlinks" ;;
    emacs) echo "task_emacs" ;;
    *) return 1 ;;
  esac
}

# Default task order for full installation
DEFAULT_TASKS=(homebrew packages macos shell wm devtools symlinks emacs)

#
# Main Execution
#

function show_usage() {
  cat << EOF
Usage: $0 <action> [tasks...] [options]

Actions:
  install           Run installation tasks
  upgrade           Run upgrade tasks

Tasks (run all if none specified):
  homebrew          Install/update Homebrew
  packages          Install packages from Brewfiles
  macos             Configure macOS defaults
  shell             Set up fish shell
  wm                Configure yabai + skhd
  devtools          Set up git, ssh, gpg
  symlinks          Create symlinks
  emacs             Set up Emacs (supports emacs:subtask syntax)

Options:
  --dry-run         Show what would be done without doing it
  --force           Skip dependency checks
  -h, --help        Show this help

Examples:
  $0 install                    # Full installation
  $0 install packages wm        # Just packages and window manager
  $0 upgrade packages           # Upgrade packages only
  $0 install emacs:config       # Run specific Emacs subtask
  $0 install --dry-run          # See what would happen

EOF
}

function main() {
  # Parse action
  ACTION=${1:-}
  case "$ACTION" in
    install|upgrade)
      shift
      ;;
    -h|--help|help)
      show_usage
      exit 0
      ;;
    "")
      error "No action specified"
      show_usage
      exit 1
      ;;
    *)
      error "Unknown action: $ACTION"
      show_usage
      exit 1
      ;;
  esac

  # Parse options and tasks
  DRY_RUN=false
  FORCE=false
  SELECTED_TASKS=()
  EMACS_SUBTASKS=()

  while [[ $# -gt 0 ]]; do
    case "$1" in
      --dry-run)
        DRY_RUN=true
        shift
        ;;
      --force)
        FORCE=true
        shift
        ;;
      -h|--help)
        show_usage
        exit 0
        ;;
      emacs:*)
        SELECTED_TASKS+=(emacs)
        EMACS_SUBTASKS+=("${1#emacs:}")
        shift
        ;;
      *)
        if is_valid_task "$1"; then
          SELECTED_TASKS+=("$1")
        else
          error "Unknown task: $1"
          show_usage
          exit 1
        fi
        shift
        ;;
    esac
  done

  # Use default tasks if none specified
  if [[ ${#SELECTED_TASKS[@]} -eq 0 ]]; then
    SELECTED_TASKS=("${DEFAULT_TASKS[@]}")
  fi

  # Show greeting
  show_greeting

  # Show configuration
  log "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  info "Bootstrap Configuration"
  log "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  log "  Action:       $ACTION"
  log "  OS:           $OS_NAME $OS_VERSION"
  log "  User:         $USER"
  log "  Hostname:     $(hostname -s)"
  log "  Tasks:        ${SELECTED_TASKS[*]}"
  if [[ ${#EMACS_SUBTASKS[@]} -gt 0 ]]; then
    log "  Emacs:        ${EMACS_SUBTASKS[*]}"
  fi
  if [[ "$DRY_RUN" == "true" ]]; then
    log "  Mode:         ${YELLOW}DRY RUN${RESET}"
  fi
  log "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  log ""

  # Acquire lock
  if [[ "$DRY_RUN" != "true" ]]; then
    acquire_lock
  fi

  # Run tasks
  local failed_tasks=()
  local completed_tasks=()

  for task in "${SELECTED_TASKS[@]}"; do
    if ! "$(get_task_function "$task")"; then
      failed_tasks+=("$task")
    else
      completed_tasks+=("$task")
    fi
    log ""
  done

  # Summary
  log "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  info "Summary"
  log "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

  if [[ ${#completed_tasks[@]} -gt 0 ]]; then
    success "Completed: ${completed_tasks[*]}"
  fi

  if [[ ${#failed_tasks[@]} -gt 0 ]]; then
    error "Failed: ${failed_tasks[*]}"
    exit 1
  fi

  if [[ "$DRY_RUN" == "true" ]]; then
    log ""
    info "This was a dry run. Run without --dry-run to apply changes."
  fi

  show_farewell
  success "The theme is complete!"
}

main "$@"
