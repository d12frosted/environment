#!/usr/bin/env bash

set -e

function error() {
  echo -e "\033[0;31m$@\033[0m"
}

if [[ "$(uname)" != "Darwin" ]]; then
  error "unsupported operating system: $(uname). This script works on Darwin only."
  exit 1
fi

target=$XDG_CONFIG_HOME
if [[ "$target" = "" ]]; then
  target="$HOME/.config"
fi

repo="git@github.com:d12frosted/environment.git"
if [[ "$USE_HTTPS" = "true" ]]; then
  repo="https://github.com/d12frosted/environment.git"
fi

if [[ -d "$target" ]]; then
  echo "$target already exists"
else
  git clone "$repo" "$target"
fi

function ensureDir() {
  if [[ ! -d "$1" ]]; then
    echo "create $1"
    mkdir -p "$1"
  fi
}

ensureDir "$HOME/.local/bin"

function check() {
  command -v "$1" >/dev/null 2>&1
}

check brew || {
  /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
  brew update
}

function safe_link() {
  s="$target/$1"
  t="${2/#\~/$HOME}"
  d=$(dirname $t)
  if [[ ! -f "$s" ]]; then
    error "can not link '$s' as it does not exist"
    exit 1
  fi

  if [[ ! -d $d ]]; then
    echo "create $d"
    mkdir -p $d
  fi

  if [[ -L "$t" ]]; then
    echo "relink $s -> $t"
    rm "$t"
  else
    echo "link $s -> $t"
  fi

  ln -s "$s" "$t"
}

while IFS='' read -r line || [[ -n "$line" ]]; do
  safe_link $line
done < "$target/bootstrap/Linkfile"

cd "$target/bootstrap" && brew bundle

echo "set -U XDG_CONFIG_HOME ~/.config" | fish
echo "set -x XDG_CONFIG_HOME ~/.config" | fish
echo "set -x SPACEMACSDIR $XDG_CONFIG_HOME/emacs" | fish

emacs --batch -l "$target/emacs/test.el"
