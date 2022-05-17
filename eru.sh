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
    NODENAME=$(uname --nodename)
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

export PATH=/opt/homebrew/bin:$PATH
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
    sh <(curl -L https://nixos.org/nix/install) --daemon
  fi
}

macos_guard && {
  theme_guard "system" "ensure brew installation" && {
    if check brew; then
      echo "Found brew executable at $(which brew)"
      echo "Nothing to do"
    else
      section "install brew"
      /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

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
      nix build --impure \
        ./#darwinConfigurations.${fellow}.system
      result/sw/bin/darwin-rebuild switch \
        --impure \
        --flake ./#${fellow}
    }
    nixos_guard && {
      sudo nixos-rebuild switch --flake .
    }
    linux_guard && ! nixos_guard && {
      nix build \
        --experimental-features 'nix-command flakes' \
        ./#homeConfigurations.borysb.activationPackage
      ./result/activate switch
    }
  }

  # cleanup if needed
  LAST_NIX_CLEANUP_FILE="${XDG_CACHE_HOME}/eru/last_nix_cleanup"
  if [ ! -f "$LAST_NIX_CLEANUP_FILE" ]; then
    date '+%F' > "$LAST_NIX_CLEANUP_FILE"
  fi
  LAST_NIX_CLEANUP=$(date -d "$(cat "$LAST_NIX_CLEANUP_FILE")" '+%s')
  TODAY=$(date '+%s')
  DIFF_SECONDS=$(( "$TODAY" - "$LAST_NIX_CLEANUP" ))
  DIFF_DAYS=$(( "$DIFF_SECONDS" / 86400 ))
  CLEANUP_NIX_AFTER_DAYS=10
  if [ "$DIFF_DAYS" -ge "$CLEANUP_NIX_AFTER_DAYS" ]; then
     section "cleanup old nix generations"
     nix-collect-garbage -d
     date '+%F' > "$LAST_NIX_CLEANUP_FILE"
  fi
}

macos_guard && {
  theme_guard "system" "ensure yabai installation" && {
    echo "$(whoami) ALL = (root) NOPASSWD: $(which yabai) --load-sa" | sudo tee /private/etc/sudoers.d/yabai
    brew services restart yabai
  }
}

theme_guard "system" "make Eru more approachable" && {
  "$XDG_CONFIG_HOME/bin/safe_link" "$XDG_CONFIG_HOME/eru.sh" "$HOME/.local/bin/eru"
}

theme_guard "system" "Fix gnupg" && {
  # make sure that I am the owner
  chown -R "$(whoami)" ~/.gnupg/
  # correct permissions
  find ~/.gnupg -type f -exec chmod 600 {} \;
  find ~/.gnupg -type d -exec chmod 700 {} \;
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

theme_guard "haskell" "ensure stack installation" && {
  check stack || {
    ghcup install stack
    stack config set install-ghc false --global
    stack config set system-ghc  true  --global
  }
}

theme_guard "haskell" "ensure HLS installation" && {
  check haskell-language-server-wrapper || {
    ghcup install hls
  }
}

linux_guard && {
  theme_guard "xmonad" "Rebuild Xmonad configurations" && {
    section "Install xmonad"
    (
      cd "$XDG_CONFIG_HOME/xmonad"

      if nixos_guard; then
        echo "Build d12-xmonad"
        nix build .#d12x:exe:d12-xmonad
        rm -f "$HOME/.local/bin/d12-xmonad"
        cp ./result/bin/d12-xmonad "$HOME/.local/bin/d12-xmonad"
        chown "$USER" "$HOME/.local/bin/d12-xmonad"
        chgrp wheel "$HOME/.local/bin/d12-xmonad"

        echo "Build d12-xmobar"
        nix build .#d12x:exe:d12-xmobar
        rm -f "$HOME/.local/bin/d12-xmobar"
        cp ./result/bin/d12-xmobar "$HOME/.local/bin/d12-xmobar"
        chown "$USER" "$HOME/.local/bin/d12-xmobar"
        chgrp wheel "$HOME/.local/bin/d12-xmobar"
      else
        cabal install --installdir="$HOME/.local/bin" --overwrite-policy=always
      fi
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
  if [ ! -f "$eldev_bin" ]; then
    curl -fsSL https://raw.github.com/doublep/eldev/0.10.3/bin/eldev > "$eldev_bin"
  fi
  chmod a+x "$eldev_bin"
  eldev --version
}

install_guard && {
  theme_guard "Emacs" "download icons" && {
    icons_dir="$XDG_CONFIG_CACHE/emacs/cache/icons"
    mkdir -p "$icons_dir"

    if [ ! -d "${icons_dir}/fontawesome" ]; then
      log "Downloading fontawesome SVGs"
      fa_version="6.1.1"
      fa_dir="fontawesome-free-${fa_version}-desktop"
      fa_zip="${fa_dir}.zip"
      fa_url="https://use.fontawesome.com/releases/v${fa_version}/${fa_zip}"
      curl -L "$fa_url" -o "${icons_dir}/${fa_zip}"
      unzip "${icons_dir}/${fa_zip}" -d "${icons_dir}"
      mv "${icons_dir}/${fa_dir}" "${icons_dir}/fontawesome"
    fi

    if [ ! -d "${icons_dir}/bootstrap" ]; then
      log "Downloading bootstrap SVGs"
      twbs_version="1.8.2"
      twbs_dir="bootstrap-icons-${twbs_version}"
      twbs_zip="${twbs_dir}.zip"
      twbs_url="https://github.com/twbs/icons/releases/download/v${twbs_version}/${twbs_zip}"
      curl -L "$twbs_url" -o "${icons_dir}/${twbs_zip}"
      unzip "${icons_dir}/${twbs_zip}" -d "${icons_dir}"
      mv "${icons_dir}/${twbs_dir}" "${icons_dir}/bootstrap"
    fi
  }

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

