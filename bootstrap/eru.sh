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
#   ./eru.sh linking repositories brew
#

#
# Fast failure
#

set -e

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
  echo -e "\033[0;30m$*\033[0m"
}

function section() {
  echo -e "\033[0;34m=> $*\033[0m"
}

function theme() {
  echo -e "\033[1;32m=> $1 Theme :: ${@:2}\033[0m"
}

function inactive_theme() {
  echo -e "\033[1;37m=> $1 Theme :: ${@:2}\033[0m"
}

#
# Greetings
#

intro "Of the theme that I have declared to you, I will now that ye make in harmony
together a Great Music. And since I have kindled you with the Flame
Imperishable, ye shall show forth your powers in adorning this theme, each with
his own thoughts and devices, if he will. But I win sit and hearken, and be glad
that through you great beauty has been wakened into song."
intro

#
# Operating system check
#

theme "Guardian" "Checking operating system"
supported_os="Darwin"
if [[ "$(uname)" != "$supported_os" ]]; then
  error "unsupported operating system: $(uname). This script works on $supported_os only."
  exit 1
fi

#
# Helpers
#

theme "Supporting" "Defining helpers"

function theme_guard() {
  key=$(echo "$1" | awk '{print tolower($0)}')
  guard=$(eval echo "\$guard_$key")
  if [[ "$ALL" = "true" || "$guard" = "true" ]]; then
    theme "$1" "${@:2}"
    return 0
  else
    inactive_theme "$1" "${@:2}"
    return 1
  fi
}

function require_repo() {
  if [[ -d "$1/.git" ]]; then
    log "$1 already exists"
  else
    git clone "$2" "$1"
  fi
  if [[ "$3" != "" ]]; then
    cd "$1" && git checkout "$3"
  fi
}

function require_github_repo() {
  if [[ "$USE_HTTPS" = "true" ]]; then
    require_repo "$1" "https://github.com/$2.git" "${@:3}"
  else
    require_repo "$1" "git@github.com:$2.git" "${@:3}"
  fi
}

function sync_repo() {
  section "sync_repo $*"

  wd=$(eval echo "$1")
  require_github_repo "$wd" "${@:2}"

  # branch="$3"
  # if [[ $branch = "" ]]; then
  #   branch="master"
  # fi

  # remote=origin
  # remote_url="git@github.com:$2.git"

  # if [[ "$USE_HTTPS" = "true" ]]; then
  #   remote_url="https://github.com/$2.git"
  # fi

  # cd "$wd" && {
  #   git diff-index --quiet HEAD -- || {
  #     echo "Your working directory is not clean."
  #     echo "Please commit or stash all changes before proceeding."
  #     exit 1
  #   }

  #   current_branch=$(git symbolic-ref --short HEAD)
  #   if [[ $branch != "$current_branch" ]]; then
  #     echo "Switching from $current_branch to $branch"
  #     git checkout "$branch"
  #   fi

  #   if [[ -d .git/refs/remotes/$remote ]]; then
  #     url=$(git remote get-url $remote)
  #     if [[ $url != "$remote_url" ]]; then
  #       echo "Remote '$branch' has wrong url, so updating it"
  #       echo "  $url -> $remote_url"
  #       git remote set-url $remote "$remote_url"
  #     fi
  #   else
  #     echo "Could not find remote '$remote', so adding it"
  #     git remote add $remote "$remote_url"
  #   fi

  #   git fetch $remote
  #   if [[ $(git rev-parse HEAD) == $(git rev-parse $remote/$branch) ]]; then
  #     echo "Everything up-to-date"
  #     exit 0
  #   fi
  #   echo "Fetched changes:"
  #   git --no-pager lg HEAD..$remote/$branch
  #   git reset --hard $remote/$branch
  # }
}

function ensure_dir() {
  if [[ ! -d "$1" ]]; then
    log "create $1"
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
    log "create $d"
    mkdir -p "$d"
  fi

  if [[ -L "$t" ]]; then
    log "relink $s -> $t"
    rm "$t"
  else
    log "link $s -> $t"
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

theme "Supporting" "Defining variables"

target=$XDG_CONFIG_HOME
if [[ "$target" = "" ]]; then
  target="$HOME/.config"
fi
XDG_CONFIG_HOME=$target

DEVELOPER=$HOME/Developer
if [[ "$USER" != "d12frosted" ]]; then
  DEVELOPER=$HOME/Developer/personal
fi

ALL="true"

POSITIONAL=()
while [[ $# -gt 0 ]]
do
  key=$(echo "$1" | awk '{print tolower($0)}')
  declare -r "guard_$key=true"
  shift
done
set -- "${POSITIONAL[@]}" # restore positional parameters

if [[ "$INTERACTIVE" = "" ]]; then
  INTERACTIVE=true
fi

#
# Actual bootstrap
#

theme "Guardian" "Ensure all directories exists"
ensure_dir "$HOME/.local/bin"
ensure_dir "$DEVELOPER"
ensure_dir "$HOME/Dropbox/Apps/Emacs"

theme_guard "SSH" "Checking SSH keys" && {
  if [[ "$INTERACTIVE" = "true" ]]; then
    ssh_key_add_url="https://github.com/settings/ssh/new"
    ssh_key_path="$HOME/.ssh/id_rsa"
    ssh_key_pub_path="${ssh_key_path}.pub"
    ssh_config_path="$HOME/.ssh/config"

    if [[ -f "$ssh_key_path" ]]; then
      log "SSH key found at $ssh_key_path."
    else
      log "No SSH key found."
      mkdir -p $(dirname "$ssh_key_path")
      ssh-keygen -t rsa -b 4096 -C "$USER" -f "$ssh_key_path"
      log "SSH key was generated."
    fi

    log "Starting ssh-agent"
    eval "$(ssh-agent -s)"

    log "Automatically load SSH key and use Keychain"
    log "Host *
 AddKeysToAgent yes
 UseKeychain yes
 IdentityFile $ssh_key_path" > "$ssh_config_path"

    log "Add SSH key to ssh-agent"
    ssh-add -K ~/.ssh/id_rsa

    log "Make sure to add SSH key to GitHub"
    pbcopy < "$ssh_key_pub_path"
    open "$ssh_key_add_url"
    read -p "Press enter to continue"
  fi
}

theme_guard "Repositories" "Sync repositories from Repofile" && {
  map_lines sync_repo "$target/bootstrap/Repofile"
}

theme_guard "Linking" "Link all files as defined in Linkfile" && {
  map_lines safe_link "$target/bootstrap/Linkfile"
}

theme_guard "Brew" "Ensure brew exists" && {
  check brew || {
    info "Installing brew"
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    brew update
  }
}

theme_guard "Brew" "Install all dependencies" && {
  cd "$target/bootstrap" && brew bundle
}

theme "Fish" "Setup fish variables"
echo "set -U XDG_CONFIG_HOME $target" | fish
echo "set -x XDG_CONFIG_HOME $target" | fish
echo "set -x SPACEMACSDIR $XDG_CONFIG_HOME/emacs" | fish

theme "Git" "Create a local git config file"
touch "$target/git/local.config"

theme_guard "macOS" "Write all defaults" && {
  source "$target/macos/defaults.sh"
}

theme_guard "skhd" "Patch skhd application PATH" && {
  check skhd && {
    "$target/utils/bin/patch_skhd_path"
  }
}

theme_guard "Guardian" "Check that Emacs runs as expected" && {
  emacs --batch -l "$target/emacs/test.el"
}

true
