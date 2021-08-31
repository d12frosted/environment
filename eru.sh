#!/usr/bin/env bash
#
################################################################################
#
# Of the theme that I have declared to you, I will now that ye make in harmony
# together a Great Music. And since I have kindled you with the Flame
# Imperishable, ye shall show forth your powers in adorning this theme, each
# with his own thoughts and devices, if he will. But I win sit and hearken, and
# be glad that through you great beauty has been wakened into song.
#
#   John Ronald Reuel Tolkien (c)
#
################################################################################
#
# Run this script to install all dependencies and configurations. If you wish to
# perform only specific task or tasks pass them as arguments, space-separated.
#
#   ./eru.sh [theme] [theme] ...
#
# For example,
#
#   ./eru.sh linking repositories packages
#

#
# Hi, my name is
#

fellow="d12frosted"

#
# Fast failure
#

set -e

#
# Get the OS info
#

KERNEL_NAME=$(uname -s | tr '[:upper:]' '[:lower:]')
KERNEL_RELEASE=$(uname -r | tr '[:upper:]' '[:lower:]')
NODENAME=$(uname --nodename)
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
    case $NODENAME in
      nixos)
        OS_NAME="nixos"
        ;;
    esac
    ;;
  *)
    ;;
esac

#
# Setup USER
#

if [ -z "$USER" ]; then
  USER=$(whoami)
fi

#
# Setup PATH
#

mkdir -p "$HOME/.local/bin/"
export PATH=$HOME/.local/bin:$PATH
export XDG_CONFIG_HOME=${GITHUB_WORKSPACE:-${XDG_CONFIG_HOME:-$HOME/.config}}
export XDG_CONFIG_CACHE="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export PATH=$XDG_CONFIG_HOME/bin:$PATH

export DEVELOPER=$HOME/Developer
if [[ "$USER" != "$fellow" ]]; then
  export DEVELOPER=$HOME/Developer/personal
fi

#
# Logging
#

function error() {
  echo -e "\033[0;31m$*\033[0m"
}

function intro() {
  echo -e "\033[0;34m$*\033[0m"
}

function log() {
  echo -e "$*"
}

function section() {
  echo -e "\033[0;34m=> $*\033[0m"
}

function a_theme() {
  local text=">>> $2 :: ${*:3}"
  local length="${#text}"
  echo
	echo '┌────────────────────────────────────────────────────────────────────────────┐'
	echo -ne "│ \033[$1m$text\033[0m"
	printf "%$((75 - length))s│\n"
	echo '└────────────────────────────────────────────────────────────────────────────┘'
}

function optional_theme() {
  a_theme "1;32" "$1" "${*:2}"
}

function inactive_theme() {
  a_theme "1;37" "$1" "${*:2}"
}

#
# Greetings
#

intro "Of the theme that I have declared to you, I will now that ye make in harmony
together a Great Music. And since I have kindled you with the Flame
Imperishable, ye shall show forth your powers in adorning this theme, each with
his own thoughts and devices, if he will. But I win sit and hearken, and be glad
that through you great beauty has been wakened into song."

log
log "Kernel name:      $KERNEL_NAME"
log "Kernel release:   $KERNEL_RELEASE"
log "Operating system: $OS_NAME"
log "OS version:       $OS_VERSION"
log "User:             $USER"
log "XDG_CONFIG_HOME:  $XDG_CONFIG_HOME"
log

#
# Helpers
#

section "Defining helpers"

function theme_guard() {
  key=$(echo "$1" | tr '[:upper:]' '[:lower:]')
  local guard_ref="guard_$key"
  local ignore_guard_ref="guard_ignore_$key"
  guard="${!guard_ref}"
  ignore_guard="${!ignore_guard_ref}"
  if [[ ("$ALL" = "true" || "$guard" = "true") && "$ignore_guard" = "" ]]; then
    optional_theme "$1" "${@:2}"
    return 0
  else
    inactive_theme "$1" "${@:2}"
    return 1
  fi
}

function install_guard() {
  [[ "$ACTION" == "install" ]]
  return
}

function upgrade_guard() {
  [[ "$ACTION" == "upgrade" ]]
  return
}

function test_guard() {
  [[ "$ACTION" == "test" ]]
  return
}

function nixos_guard() {
  [[ "$OS_NAME" == "nixos" ]]
  return
}

function linux_guard() {
  [[ "$KERNEL_NAME" == "linux" ]]
  return
}

function macos_guard() {
  [[ "$OS_NAME" == "macos" ]]
  return
}

function check() {
  command -v "$1" >/dev/null 2>&1
}

#
# Setup variables
#

section "Defining variables"

ALL="true"
ACTION=
case $1 in
  install|upgrade|test)
    ACTION=$1
    shift
    ;;
  *)
    if [ -z "$1" ]; then
      ACTION=install
    else
      error "action '$1' is not supported"
      log "supported actions are: install, upgrade, test"
      exit 1
    fi
    ;;
esac

POSITIONAL=()
while [[ $# -gt 0 ]]
do
  if [[ "$1" != "" ]]; then
    if [[ "$1" = -* ]]; then
      key=$(echo "${1#-}" | tr '[:upper:]' '[:lower:]')
      declare -r "guard_ignore_$key=true"
    else
      key=$(echo "$1" | tr '[:upper:]' '[:lower:]')
      declare -r "guard_$key=true"
      ALL="false"
    fi
  fi
  shift
done
set -- "${POSITIONAL[@]}" # restore positional parameters

if [[ "$INTERACTIVE" = "" ]]; then
  INTERACTIVE=true
fi

#
# Lock
#

LOCK_FILE=$XDG_CACHE_HOME/eru/eru.lock
if [ -f "$LOCK_FILE" ]; then
  error "
Yet another world is being shaped by Eru

One must either wait patiently or embrace the horrors of the unknown and
manually delete the $LOCK_FILE"
  exit 1
fi
mkdir -p "$(dirname "$LOCK_FILE")"
touch "$LOCK_FILE"

function unlock() {
  rm -rf "$LOCK_FILE"
}

trap unlock INT TERM EXIT

#
# Actual bootstrap
#

export PATH=/nix/var/nix/profiles/default/bin:$PATH
export PATH=/run/current-system/sw/bin:$PATH
export PATH=$HOME/.nix-profile/bin:$PATH
export PATH=/run/wrappers/bin:$PATH

theme_guard "system" "ensure nix installation" && {
  if check nix; then
    echo "Found nix executable at $(which nix)"
    echo "Nothing to do"
  else
    section "install nix"
    macos_guard && {
      # TODO: remove once nix 2.4 lands
      sh <(curl https://abathur-nix-install-tests.cachix.org/serve/yihf8zbs0jwph2rs9qfh80dnilijxdi2/install) --tarball-url-prefix https://abathur-nix-install-tests.cachix.org/serve
      # sh <(curl -L https://nixos.org/nix/install) --daemon
    }
  fi
  # TODO: remove once nix 2.4 lands
  nixos_guard && {
    if [ -z "$(nix-channel --list)" ]; then
      nix-channel --add https://nixos.org/channels/nixpkgs-unstable unstable
      nix-channel --update
      nix-env -iA unstable.nixUnstable
    fi
  }
}

upgrade_guard && {
  theme_guard "system" "upgrade nix environment" && {
    nix-channel --update
    cd "$XDG_CONFIG_HOME" && nix flake update
  }
}

theme_guard "system" "build nix environment" && {
  cd "$XDG_CONFIG_HOME" && {
    macos_guard && {
      nix build \
        --experimental-features "nix-command flakes" --impure \
        -I hm-config="$XDG_CONFIG_HOME/nix/home.nix" \
        ./#darwinConfigurations.${fellow}.system
      result/sw/bin/darwin-rebuild switch \
        --impure \
        -I hm-config="$XDG_CONFIG_HOME/nix/home.nix" \
        --flake ./#${fellow}
    }
    nixos_guard && {
      sudo nixos-rebuild switch --flake .
    }
  }
}

theme_guard "system" "make Eru more approachable" && {
  "$XDG_CONFIG_HOME/bin/safe_link" "$XDG_CONFIG_HOME/eru.sh" "$HOME/.local/bin/eru"
}

export GHCUP_USE_XDG_DIRS=1
theme_guard "haskell" "ensure ghcup installation" && {
  check ghcup || {
    # https://www.haskell.org/ghcup/
    export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
  }

  upgrade_guard && {
    ghcup upgrade
  }
}

theme_guard "haskell" "ensure HLS installation" && {
  check haskell-language-server-wrapper || {
    ghcup install hls
  }
}

nixos_guard && {
  theme_guard "xmonad" "Rebuild Xmonad configurations" && {
    section "Install xmonad"
    (
      cd "$XDG_CONFIG_HOME/xmonad"

      echo "Build d12-xmonad"
      nix build .#d12x:exe:d12-xmonad
      rm "$HOME/.local/bin/d12-xmonad"
      cp ./result/bin/d12-xmonad "$HOME/.local/bin/d12-xmonad"
      chown "$USER" "$HOME/.local/bin/d12-xmonad"
      chgrp wheel "$HOME/.local/bin/d12-xmonad"

      echo "Build d12-xmobar"
      nix build .#d12x:exe:d12-xmobar
      rm "$HOME/.local/bin/d12-xmobar"
      cp ./result/bin/d12-xmobar "$HOME/.local/bin/d12-xmobar"
      chown "$USER" "$HOME/.local/bin/d12-xmobar"
      chgrp wheel "$HOME/.local/bin/d12-xmobar"
    )

    section "Restart xmonad"
    if pgrep d12-xmonad; then
      log "Found running instance of xmonad. Restarting..."
      d12-xmonad --restart
    else
      log "No running instance of xmonad is found. Meh..."
    fi
  }
}

theme_guard "Emacs" "setup Eldev" && {
  eldev_bin=$HOME/.local/bin/eldev
  curl -fsSL https://raw.github.com/doublep/eldev/master/bin/eldev > "$eldev_bin"
  chmod a+x "$eldev_bin"
}

install_guard && {
  theme_guard "Emacs" "setup Emacs configurations" && {
    mkdir -p "$XDG_CACHE_HOME/emacs/etc"
    cd "$XDG_CONFIG_HOME/emacs" && {
      make bootstrap compile lint vulpea
    }
  }
}

upgrade_guard && {
  theme_guard "Emacs" "upgrade Emacs packages" && {
    cd "$XDG_CONFIG_HOME/emacs" && {
      make upgrade compile lint
    }
  }
}

test_guard && {
  theme_guard "Emacs" "test Emacs configurations" && {
    cd "$XDG_CONFIG_HOME/emacs" && {
      make test
    }
  }
}

true

