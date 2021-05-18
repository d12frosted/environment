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

export PATH=$HOME/.local/bin:$PATH

target=$HOME/.config
if [[ -d "$XDG_CONFIG_HOME" ]]; then
  target="$XDG_CONFIG_HOME"
fi
if [[ -d "$GITHUB_WORKSPACE" ]]; then
  target="$GITHUB_WORKSPACE"
fi

export XDG_CONFIG_HOME=$target
export XDG_CONFIG_CACHE="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

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
  echo
	echo '┌────────────────────────────────────────────────────────────────────────────┐'
	echo -ne "│ \033[$1m$text\033[0m"
	printf "%$((76 - $(echo $text | wc -c)))s│\n"
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

function arch_guard() {
  [[ "$OS_NAME" == "arch" ]]
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

section "Make Eru more approachable"
$XDG_CONFIG_HOME/bin/safe_link $XDG_CONFIG_HOME/eru.sh $HOME/.local/bin/eru

theme_guard "system" "ensure nix installation" && {
  check nix && {
    echo "Found nix executable at $(which nix)"
    echo "Nothing to do"
  } || {
    install_script="$(mktemp -d)/install"
    curl -L https://nixos.org/nix/install -o "$install_script"
	  chmod +x "$install_script"
	  "$install_script" --daemon
  }
}

theme_guard "system" "build nix environment" && {
  result="$(mktemp -d)/result"
  echo "building to $result"
  nix build \
    --experimental-features "nix-command flakes" --impure \
    -I hm-config=$XDG_CONFIG_HOME/modules/home.nix \
    -o "$result" \
    $XDG_CONFIG_HOME/#darwinConfigurations.${fellow}.system
  echo "switching environment"
  "$result"/sw/bin/darwin-rebuild switch \
    --impure \
    -I hm-config=$XDG_CONFIG_HOME/modules/home.nix \
    --flake $XDG_CONFIG_HOME/#${fellow}
}

arch_guard && {
  theme_guard "packages" "Bootstrap Arch Linux" && {
    section "Install crutial dependenices"
    # sudo pacman -Syu --noconfirm
    sudo pacman -S --noconfirm --needed base-devel git pacman-contrib

    section "Rank mirrors for pacman"
    mirrorlist="/etc/pacman.d/mirrorlist"
    mirrorlist_bak="${mirrorlist}.bak"
    if [[ -f "$mirrorlist_bak" ]]; then
      log "Not updating mirrors list, because '$mirrorlist_bak' exists"
      log "Delete in order to re-rank mirrors"
    else
      mirrorlist_tmp=$(mktemp)
      curl -s 'https://www.archlinux.org/mirrorlist/?country=all&protocol=https&ip_version=4' \
        | sed -e 's/^#Server/Server/' -e '/^#/d' > "$mirrorlist_tmp"
      sudo cp "$mirrorlist_tmp" "$mirrorlist_bak"
      # shellcheck disable=SC2024
      sudo sh -c "rankmirrors -n 6 '$mirrorlist_bak' > '$mirrorlist'"
    fi

    section "Install aura for simpler AUR access"
    check aura || {
      sudo mkdir -p /var/cache/pacman/pkg
      aura_dir=$(mktemp -d)
      git clone https://aur.archlinux.org/aura-bin.git "$aura_dir"
      cd "$aura_dir" && {
        makepkg -si --noconfirm
      }
    }

    section "Install yay for simpler AUR access"
    check yay || {
      yay_dir=$(mktemp -d)
      git clone https://aur.archlinux.org/yay.git "$yay_dir"
      cd "$yay_dir" && {
        makepkg -si --noconfirm
      }
    }
  }

  install_guard && {
    theme_guard "packages" "Install all dependencies" && {
      log "Import known GPG keys"
      # spotify
      curl -sS https://download.spotify.com/debian/pubkey.gpg | gpg --import

      function combine_files {
        local output
        output=$(mktemp)
        for f in "$@"; do
          if [[ -f $f ]]; then
            cat "$f" >> "$output"
          fi
        done
        echo "$output"
      }

      log "Install packages"

      pacman_file=$(combine_files "$target/arch/Pacmanfile" "$target/arch/Pacmanfile_$USER")
      pacman_ignore=$(combine_files "$target/arch/Pacmanignore" "$target/arch/Pacmanignore_$USER")
      # shellcheck disable=SC2046
      sudo aura -S --noconfirm --needed $(comm -23 "$pacman_file" "$pacman_ignore")

      aur_file=$(combine_files "$target/arch/Aurfile" "$target/arch/Aurfile_$USER")
      aur_ignore=$(combine_files "$target/arch/Aurignore" "$target/arch/Aurignore_$USER")
      # shellcheck disable=SC2046
      sudo aura -A --noconfirm --needed $(comm -23 "$aur_file" "$aur_ignore")
    }
  }

  upgrade_guard && {
    theme_guard "packages" "Upgrade Arch Linux" && {
      sudo aura -Syu --noconfirm
      sudo aura -Aux --noconfirm
    }
  }

  theme_guard "hardware" "Setup keyboard" && {
    if [[ ! -f /usr/share/X11/xkb/symbols/ua.bak ]]; then
      sudo mv /usr/share/X11/xkb/symbols/ua /usr/share/X11/xkb/symbols/ua.bak
    fi
    sudo cp "$XDG_CONFIG_HOME/xorg/xkb/symbols/ua" "/usr/share/X11/xkb/symbols/ua"

    # Make sure that Caps doesn't miss it's purpose.
    setxkbmap -option caps:ctrl_modifier
  }

  theme_guard "hardware" "Setup touchpad" && {
    sudo cp "$XDG_CONFIG_HOME/xorg/30-touchpad.conf" "/etc/X11/xorg.conf.d/30-touchpad.conf"
  }

  theme_guard "hardware" "Setup autolock" && {
    sudo cp "$XDG_CONFIG_HOME/arch/lock@.service" /etc/systemd/system/lock@.service
    systemctl enable "lock@${USER}.service" || error "systemd is not working"
  }

  theme_guard "hardware" "Setup backlight rules" && {
    tmp_rule=$(mktemp)
    for backlight in /sys/class/backlight/*; do
      name=$(basename "$backlight")
      echo "ACTION==\"add\", SUBSYSTEM==\"backlight\", KERNEL==\"$name\", RUN+=\"/bin/chgrp video /sys/class/backlight/%k/brightness\"" >> "$tmp_rule"
      echo "ACTION==\"add\", SUBSYSTEM==\"backlight\", KERNEL==\"$name\", RUN+=\"/bin/chmod g+w /sys/class/backlight/%k/brightness\"" >> "$tmp_rule"
    done
    sudo cp "$tmp_rule" /etc/udev/rules.d/backlight.rules
    if id -nG "$USER" | grep -qw "video"; then
      echo "You are already able to adjust brightness level"
    else
      echo "Adding you to 'video' user group"
      sudo gpasswd -a "$USER" video
    fi
  }

  theme_guard "gnupg" "Fix permissions" && {
    # make sure that I am the owner
    chown -R "$(whoami)" ~/.gnupg/
    # correct permissions
    find ~/.gnupg -type f -exec chmod 600 {} \;
    find ~/.gnupg -type d -exec chmod 700 {} \;
  }
}

arch_guard && {
  theme_guard "xmonad" "Rebuild Xmonad configurations" && {
    section "Install xmonad"
    (
      cd "$XDG_CONFIG_HOME/xmonad"
      stack --allow-different-user install || {
        notify send -a "Eru" -t "Failed to compile xmonad" -u critical
      }
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

