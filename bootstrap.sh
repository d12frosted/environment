#!/usr/bin/env bash
#
# Bootstrap a fresh macOS machine from zero: no git, no Xcode CLT, no keys.
#
#   bash -c "$(curl -fsSL https://raw.githubusercontent.com/d12frosted/environment/master/bootstrap.sh)"
#
# What it does:
#   1. Installs Xcode Command Line Tools (provides git)
#   2. Clones this repository to ~/.config over HTTPS (no SSH keys needed)
#   3. Runs the full eru installation
#
# Git identity comes with the clone (git/config is the XDG git config), so
# nothing needs to be configured by hand first.
#
# Environment overrides:
#   ENVIRONMENT_REPO       clone URL (default: public HTTPS URL of this repo)
#   ERU_BOOTSTRAP_NO_INSTALL  set to 1 to clone only, without running eru

set -euo pipefail

REPO_URL="${ENVIRONMENT_REPO:-https://github.com/d12frosted/environment.git}"
TARGET="${XDG_CONFIG_HOME:-$HOME/.config}"

GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RESET='\033[0m'

function info() { echo -e "${BLUE}→ $*${RESET}"; }
function success() { echo -e "${GREEN}✓ $*${RESET}"; }
function warn() { echo -e "${YELLOW}⚠ $*${RESET}"; }

#
# 1. Xcode Command Line Tools (provides git)
#

if xcode-select -p &> /dev/null; then
  info "Xcode Command Line Tools already installed"
else
  info "Installing Xcode Command Line Tools (a dialog will pop up)..."
  xcode-select --install || true
  info "Waiting for installation to finish..."
  until xcode-select -p &> /dev/null; do
    sleep 5
  done
  success "Xcode Command Line Tools installed"
fi

#
# 2. Clone the repository to ~/.config
#
# ~/.config usually already exists on a fresh machine (applications drop
# files there early), so a plain `git clone` would refuse to run. Instead,
# init in place, fetch, and force-checkout master. Anything that collides
# with tracked files gets overwritten; on a fresh machine that is app junk.
#

if [[ -d "$TARGET/.git" ]]; then
  info "Repository already cloned at $TARGET"
else
  info "Cloning $REPO_URL to $TARGET..."
  mkdir -p "$TARGET"
  git -C "$TARGET" init -b master
  git -C "$TARGET" remote add origin "$REPO_URL"
  git -C "$TARGET" fetch origin master
  git -C "$TARGET" checkout -f -B master origin/master
  success "Cloned to $TARGET"
fi

#
# 3. Run eru
#

if [[ "${ERU_BOOTSTRAP_NO_INSTALL:-0}" == "1" ]]; then
  warn "Skipping installation (ERU_BOOTSTRAP_NO_INSTALL=1). Run: cd $TARGET && ./eru.sh install"
else
  cd "$TARGET"
  ./eru.sh install
fi

#
# 4. Next steps that need a human
#

echo ""
success "Bootstrap complete!"
echo ""
info "Next steps:"
echo "  1. Set up GitHub SSH access (add the generated key, or configure gpg-agent)"
echo "  2. Switch the remote to SSH:"
echo "       git -C $TARGET remote set-url origin git@github.com:d12frosted/environment.git"
echo "  3. Pull private config (CLAUDE.md, Claude memory):"
echo "       eru install private symlinks"
echo "  4. Copy brew/\$USER.Brewfile from an old laptop (it is gitignored), then:"
echo "       eru install packages"
