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

ALL=YES
REPO=NO
LINK=NO
BREW=NO
MACOS=NO
SKHD=NO
TESTS=NO

POSITIONAL=()

while [[ $# -gt 0 ]]
do
  key="$1"

  case $key in
    repo)
      ALL=NO
      REPO=YES
      shift # past argument
      ;;
    link)
      ALL=NO
      LINK=YES
      shift # past argument
      ;;
    brew)
      ALL=NO
      BREW=YES
      shift # past argument
      ;;
    macos)
      ALL=NO
      MACOS=YES
      shift # past argument
      ;;
    skhd)
      ALL=NO
      SKHD=YES
      shift # past argument
      ;;
    tests)
      ALL=NO
      TESTS=YES
      shift # past argument
      ;;
    *)    # unknown option
      POSITIONAL+=("$1") # save it in an array for later
      shift # past argument
      ;;
  esac
done
set -- "${POSITIONAL[@]}" # restore positional parameters

################################################################################
# Actual bootstrap
################################################################################

# install brew
check brew || {
  /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
  brew update
}

# clone dependencies
if [[ "$ALL" = "YES" || "$REPO" = "YES" ]]; then
  require_github_repo "$target" "d12frosted" "environment"
  require_github_repo "$HOME/.spacemacs" "syl20bnr" "spacemacs" "develop"
fi

# create local directory for binaries
ensure_dir "$HOME/.local/bin"
ensure_dir "$HOME/Dropbox/Apps/Emacs"

# run the Linkfile
if [[ "$ALL" = "YES" || "$LINK" = "YES" ]]; then
  map_lines "$target/bootstrap/Linkfile" safe_link
fi

# run the Brewfile
if [[ "$ALL" = "YES" || "$BREW" = "YES" ]]; then
  cd "$target/bootstrap" && brew bundle
fi

# setup fish shell
echo "set -U XDG_CONFIG_HOME $target" | fish
echo "set -x XDG_CONFIG_HOME $target" | fish
echo "set -x SPACEMACSDIR $XDG_CONFIG_HOME/emacs" | fish

# setup local git config
touch "$target/git/local.config"

# write macOS defaults
if [[ "$ALL" = "YES" || "$MACOS" = "YES" ]]; then
  "$target/macos/defaults.sh"
fi

# patch skhd
if [[ "$ALL" = "YES" || "$SKHD" = "YES" ]]; then
  check skhd && {
    "$target/utils/bin/patch_skhd_path"
  }
fi

# ensure that Emacs runs normally
if [[ "$ALL" = "YES" || "$TESTS" = "YES" ]]; then
  emacs --batch -l "$target/emacs/test.el"
fi
