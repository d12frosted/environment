#!/usr/bin/env bash

################################################################################
# Fast failure
################################################################################

set -e

function error() {
  echo -e "\033[0;31m$*\033[0m"
}

supported_os="Darwin"
if [[ "$(uname)" != "$supported_os" ]]; then
  error "unsupported operating system: $(uname). This script works on $supported_os only."
  exit 1
fi

################################################################################
# Helpers
################################################################################

function require_repo() {
  if [[ -d "$1/.git" ]]; then
     echo "$1 already exists"
  else
    git clone "$2" "$1"
  fi
  if [[ "$3" != "" ]]; then
    git checkout "$3"
  fi
}

function require_github_repo() {
  if [[ "$USE_HTTPS" = "true" ]]; then
    require_repo "$1" "https://github.com/$2/$3.git"
  else
    require_repo "$1" "git@github.com:$2/$3.git"
  fi
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
    $2 $line
  done < "$1"
}

################################################################################
# Setup variables
################################################################################

target=$XDG_CONFIG_HOME
if [[ "$target" = "" ]]; then
  target="$HOME/.config"
fi

################################################################################
# Actual bootstrap
################################################################################

# install brew
check brew || {
  /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
  brew update
}

# clone dependencies
require_github_repo "$target" "d12frosted" "environment"
require_github_repo "$HOME/.spacemacs" "syl20bnr" "spacemacs" "develop"

# create local directory for binaries
ensure_dir "$HOME/.local/bin"
ensure_dir "$HOME/Dropbox/Apps/Emacs"

# run the Linkfile
map_lines "$target/bootstrap/Linkfile" safe_link

# run the Brewfile
cd "$target/bootstrap" && brew bundle

# setup fish shell
echo "set -U XDG_CONFIG_HOME $target" | fish
echo "set -x XDG_CONFIG_HOME $target" | fish
echo "set -x SPACEMACSDIR $XDG_CONFIG_HOME/emacs" | fish

# setup local git config
touch "$target/git/local.config"

# write macOS defaults
source "$target/macos/defaults.sh"

# ensure that Emacs runs normally
emacs --batch -l "$target/emacs/test.el"
