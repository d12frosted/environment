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
#   ./eru.sh brew link
#
# Available tasks:
#
# - ssh-key
# - repo
# - link
# - brew
# - macos
# - skhd
# - tests
#

#
# Fast failure
#

set -e

function error() {
  echo -e "\033[0;31m$*\033[0m"
}

supported_os="Darwin"
if [[ "$(uname)" != "$supported_os" ]]; then
  error "unsupported operating system: $(uname). This script works on $supported_os only."
  exit 1
fi

#
# Helpers
#

function require_repo() {
  if [[ -d "$1/.git" ]]; then
     echo "$1 already exists"
  else
    git clone "$2" "$1"
  fi
  if [[ "$3" != "" ]]; then
    cd "$1" && git checkout "$3"
  fi
}

function require_github_repo() {
  if [[ "$USE_HTTPS" = "true" ]]; then
    require_repo "$1" "https://github.com/$2/$3.git" "${@:4}"
  else
    require_repo "$1" "git@github.com:$2/$3.git" "${@:4}"
  fi
}

function sync_repo() {
  require_github_repo $(eval echo "$1") "${@:2}"
}

function ensure_dir() {
  if [[ ! -d "$1" ]]; then
    echo "create $1"
    mkdir -p "$1"
  fi
}

function check() {
  command -v "$1" >/dev/null 2>&1
}

function safe_link() {
  s="$target/$1"
  shift
  t="${*/#\~/$HOME}"
  d=$(dirname "$t")

  if [[ ! -f "$s" && ! -d "$s" ]]; then
    error "can not link '$s' as it does not exist"
    exit 1
  fi

  if [[ ! -d $d ]]; then
    echo "create $d"
    mkdir -p "$d"
  fi

  if [[ -L "$t" ]]; then
    echo "relink $s -> $t"
    rm "$t"
  else
    echo "link $s -> $t"
  fi

  ln -s "$s" "$t"
}

function map_lines() {
  while IFS='' read -r line || [[ -n "$line" ]]; do
    $1 $line
  done < "$2"
}

#
# Setup variables
#

target=$XDG_CONFIG_HOME
if [[ "$target" = "" ]]; then
  target="$HOME/.config"
fi

ALL="true"
SSH_KEY="false"
REPO="false"
LINK="false"
BREW="false"
MACOS="false"
SKHD="false"
TESTS="false"

POSITIONAL=()

while [[ $# -gt 0 ]]
do
  key="$1"

  case $key in
    ssh-key)
      ALL="false"
      SSH_KEY="true"
      shift # past argument
      ;;
    repo)
      ALL="false"
      REPO="true"
      shift # past argument
      ;;
    link)
      ALL="false"
      LINK="true"
      shift # past argument
      ;;
    brew)
      ALL="false"
      BREW="true"
      shift # past argument
      ;;
    macos)
      ALL="false"
      MACOS="true"
      shift # past argument
      ;;
    skhd)
      ALL="false"
      SKHD="true"
      shift # past argument
      ;;
    tests)
      ALL="false"
      TESTS="true"
      shift # past argument
      ;;
    *)    # unknown option
      POSITIONAL+=("$1") # save it in an array for later
      shift # past argument
      ;;
  esac
done
set -- "${POSITIONAL[@]}" # restore positional parameters

#
# Actual bootstrap
#

# install brew
check brew || {
  /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
  brew update
}

# setup SSH key
if [[ "$ALL" = "true" || "$SSH_KEY" = "true" ]]; then
  ssh_key_add_url="https://github.com/settings/ssh/new"
  ssh_key_path="$HOME/.ssh/id_rsa"
  ssh_key_pub_path="${ssh_key_path}.pub"
  ssh_config_path="$HOME/.ssh/config"

  if [[ -f "$ssh_key_path" ]]; then
    echo "SSH key found at $ssh_key_path."
  else
    echo "No SSH key found."
    mkdir -p $(dirname "$ssh_key_path")
    ssh-keygen -t rsa -b 4096 -C "$USER" -f "$ssh_key_path"
    echo "SSH key was generated."
  fi

  echo "Starting ssh-agent"
  eval "$(ssh-agent -s)"

  echo "Automatically load SSH key and use Keychain"
  echo "Host *
 AddKeysToAgent yes
 UseKeychain yes
 IdentityFile $ssh_key_path" > "$ssh_config_path"

  echo "Add SSH key to ssh-agent"
  ssh-add -K ~/.ssh/id_rsa

  echo "Make sure to add SSH key to GitHub"
  pbcopy < "$ssh_key_pub_path"
  open "$ssh_key_add_url"
  read -p "Press enter to continue"
fi

# clone dependencies
if [[ "$ALL" = "true" || "$REPO" = "true" ]]; then
  map_lines sync_repo "$target/bootstrap/Repofile"
fi

# create local directory for binaries
ensure_dir "$HOME/.local/bin"
ensure_dir "$HOME/Dropbox/Apps/Emacs"

# run the Linkfile
if [[ "$ALL" = "true" || "$LINK" = "true" ]]; then
  map_lines safe_link "$target/bootstrap/Linkfile"
fi

# run the Brewfile
if [[ "$ALL" = "true" || "$BREW" = "true" ]]; then
  cd "$target/bootstrap" && brew bundle
fi

# setup fish shell
echo "set -U XDG_CONFIG_HOME $target" | fish
echo "set -x XDG_CONFIG_HOME $target" | fish
echo "set -x SPACEMACSDIR $XDG_CONFIG_HOME/emacs" | fish

# setup local git config
touch "$target/git/local.config"

# write macOS defaults
if [[ "$ALL" = "true" || "$MACOS" = "true" ]]; then
  source "$target/macos/defaults.sh"
fi

# patch skhd
if [[ "$ALL" = "true" || "$SKHD" = "true" ]]; then
  check skhd && {
    "$target/utils/bin/patch_skhd_path"
  }
fi

# ensure that Emacs runs normally
if [[ "$ALL" = "true" || "$TESTS" = "true" ]]; then
  emacs --batch -l "$target/emacs/test.el"
fi
