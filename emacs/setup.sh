#!/usr/bin/env bash
#
# Emacs configuration setup script
#
# This script handles installation, upgrade, compilation, linting, and testing
# of Emacs configuration. It can be called standalone or via eru (bootstrap.sh).
#
# Usage:
#   ./setup.sh [command...]
#
# Commands:
#   install           Bootstrap packages and generate autoloads
#   upgrade           Upgrade all packages
#   compile           Byte-compile all Emacs Lisp files
#   lint              Run linters (checkdoc, package-lint, elisp-lint)
#   test              Run tests
#   clean             Clean generated files
#   doctor            Check configuration health
#
# Examples:
#   ./setup.sh install
#   ./setup.sh compile lint
#   ./setup.sh clean install
#

set -euo pipefail

#
# Configuration
#

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RESET='\033[0m'

#
# Logging Functions
#

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
  log "${BLUE}▶ $*${RESET}"
}

function fail() {
  error "$1"
  exit 1
}

#
# Dependency Checks
#

function check_emacs() {
  if ! command -v emacs &> /dev/null; then
    fail "Emacs is not installed"
  fi

  local emacs_version
  emacs_version=$(emacs --version | head -n1 | sed 's/GNU Emacs //')
  local major_version
  major_version=$(echo "$emacs_version" | cut -d. -f1)

  if [[ "$major_version" -lt 30 ]]; then
    fail "Emacs 30.2 or higher is required (found $emacs_version)"
  fi

  info "Using Emacs $emacs_version"
}

function check_eldev() {
  if ! command -v eldev &> /dev/null; then
    warn "Eldev not found, installing..."
    curl -fsSL https://raw.github.com/doublep/eldev/master/webinstall/eldev | sh

    # Ensure eldev is in PATH
    if [[ -f "$HOME/.local/bin/eldev" ]]; then
      export PATH="$HOME/.local/bin:$PATH"
    fi

    if ! command -v eldev &> /dev/null; then
      fail "Failed to install Eldev"
    fi
  fi
}

#
# Commands
#

function cmd_install() {
  task_start "Installing Emacs packages and generating autoloads"

  check_emacs
  check_eldev

  info "Bootstrapping packages via elpaca..."
  if ! eldev -C --unstable -a -dtT build; then
    fail "Failed to bootstrap packages"
  fi

  success "Emacs packages installed and autoloads generated"
}

function cmd_upgrade() {
  task_start "Upgrading Emacs packages"

  check_emacs
  check_eldev

  info "Upgrading all packages..."
  if ! eldev -C --unstable -a -dtT upgrade; then
    fail "Failed to upgrade packages"
  fi

  success "Emacs packages upgraded"
}

function cmd_compile() {
  task_start "Byte-compiling Emacs Lisp files"

  check_emacs
  check_eldev

  info "Cleaning previous byte-compiled files..."
  eldev clean elc || true

  info "Byte-compiling all files..."
  if ! eldev -C --unstable -a -dtT compile; then
    error "Compilation failed"
    return 1
  fi

  success "All files compiled successfully"
}

function cmd_lint() {
  task_start "Running linters"

  check_emacs
  check_eldev

  info "Linting Emacs Lisp files..."
  if ! eldev -C --unstable -a -dtT lint; then
    error "Linting found issues"
    return 1
  fi

  success "All files passed linting"
}

function cmd_test() {
  task_start "Running tests"

  check_emacs
  check_eldev

  info "Running test suite..."
  if ! eldev -C --unstable -a -dtT test; then
    error "Tests failed"
    return 1
  fi

  success "All tests passed"
}

function cmd_clean() {
  task_start "Cleaning generated files"

  check_eldev

  info "Removing generated files..."
  eldev clean all

  success "Cleaned all generated files"
}

function cmd_doctor() {
  task_start "Running configuration health check"

  check_emacs

  local issues=0

  # Check for autoloads file
  if [[ ! -f "lisp/init-autoloads.el" ]]; then
    warn "Autoloads file not found (run: ./setup.sh install)"
    ((issues++))
  else
    success "Autoloads file exists"
  fi

  # Check for elpaca installation
  local elpaca_dir="${XDG_CACHE_HOME:-$HOME/.cache}/emacs/packages/$(emacs --batch --eval '(princ (format "%s.%s" emacs-major-version emacs-minor-version))')/elpaca"
  if [[ ! -d "$elpaca_dir" ]]; then
    warn "Elpaca not installed (run: ./setup.sh install)"
    ((issues++))
  else
    success "Elpaca is installed"
  fi

  # Check for Eldev
  if command -v eldev &> /dev/null; then
    success "Eldev is available"
  else
    warn "Eldev not found (will be installed automatically when needed)"
    ((issues++))
  fi

  # Check Emacs version
  local emacs_version
  emacs_version=$(emacs --version | head -n1 | sed 's/GNU Emacs //')
  local major_version
  major_version=$(echo "$emacs_version" | cut -d. -f1)

  if [[ "$major_version" -ge 30 ]]; then
    success "Emacs version is $emacs_version (>= 30.2)"
  else
    error "Emacs version is $emacs_version (< 30.2 required)"
    ((issues++))
  fi

  # Check critical files
  local critical_files=("init.el" "early-init.el" "Eldev" "lisp/config-path.el" "lisp/init-elpa.el")
  for file in "${critical_files[@]}"; do
    if [[ -f "$file" ]]; then
      success "Found $file"
    else
      error "Missing critical file: $file"
      ((issues++))
    fi
  done

  log ""
  if [[ $issues -eq 0 ]]; then
    success "Configuration health check passed"
    return 0
  else
    warn "Found $issues issue(s)"
    return 1
  fi
}

#
# Main
#

function show_usage() {
  cat << 'EOF'
Emacs Configuration Setup

Usage:
  ./setup.sh [command...]

Commands:
  install           Bootstrap packages and generate autoloads
  upgrade           Upgrade all packages
  compile           Byte-compile all Emacs Lisp files
  lint              Run linters (checkdoc, package-lint, elisp-lint)
  test              Run tests
  clean             Clean generated files
  doctor            Check configuration health

Examples:
  ./setup.sh install
  ./setup.sh compile lint
  ./setup.sh clean install
  ./setup.sh doctor

If no commands are specified, 'install' is run by default.

EOF
}

function main() {
  local commands=("$@")

  # Show help if requested
  if [[ "${1:-}" == "-h" ]] || [[ "${1:-}" == "--help" ]] || [[ "${1:-}" == "help" ]]; then
    show_usage
    exit 0
  fi

  # Default to 'install' if no commands specified
  if [[ ${#commands[@]} -eq 0 ]]; then
    commands=("install")
  fi

  log ""
  info "Emacs Configuration Setup"
  info "Working directory: $SCRIPT_DIR"
  info "Commands: ${commands[*]}"
  log ""

  local failed_commands=()
  local completed_commands=()

  for cmd in "${commands[@]}"; do
    case "$cmd" in
      install)
        if cmd_install; then
          completed_commands+=("$cmd")
        else
          failed_commands+=("$cmd")
        fi
        ;;
      upgrade)
        if cmd_upgrade; then
          completed_commands+=("$cmd")
        else
          failed_commands+=("$cmd")
        fi
        ;;
      compile)
        if cmd_compile; then
          completed_commands+=("$cmd")
        else
          failed_commands+=("$cmd")
        fi
        ;;
      lint)
        if cmd_lint; then
          completed_commands+=("$cmd")
        else
          failed_commands+=("$cmd")
        fi
        ;;
      test)
        if cmd_test; then
          completed_commands+=("$cmd")
        else
          failed_commands+=("$cmd")
        fi
        ;;
      clean)
        if cmd_clean; then
          completed_commands+=("$cmd")
        else
          failed_commands+=("$cmd")
        fi
        ;;
      doctor)
        if cmd_doctor; then
          completed_commands+=("$cmd")
        else
          failed_commands+=("$cmd")
        fi
        ;;
      *)
        error "Unknown command: $cmd"
        show_usage
        exit 1
        ;;
    esac
    log ""
  done

  # Summary
  if [[ ${#completed_commands[@]} -gt 0 ]]; then
    success "Completed: ${completed_commands[*]}"
  fi

  if [[ ${#failed_commands[@]} -gt 0 ]]; then
    error "Failed: ${failed_commands[*]}"
    exit 1
  fi

  success "All commands completed successfully"
}

main "$@"
