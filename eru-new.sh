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

set -e

function silence {
  local output=
  if ! output=$(eval "$@" 2>&1); then
    echo "$output"
    exit 1
  fi
}

#
# Haskell is the language of Eru.
#

if command -v stack >/dev/null 2>&1; then
  silence stack upgrade
else
  curl -sSL https://get.haskellstack.org/ | sh
fi

#
# Fetching the notes
#

export XDG_CONFIG_HOME=$HOME/.config

if [ ! -d "$XDG_CONFIG_HOME" ] && [ ! -d "$XDG_CONFIG_HOME/.git" ]; then
  env_https=https://github.com/d12frosted/environment
  env_ssh=git@github.com:d12frosted/environment.git

  # clone via HTTPS, as most likely SSH is not yet available or configured
  git clone $env_https "$XDG_CONFIG_HOME"

  cd "$XDG_CONFIG_HOME" && {
    git remote set-url origin $env_ssh
  }
fi

#
# Now start the Great Music
#

cd "$XDG_CONFIG_HOME/melkor" && {
  silence stack setup --allow-different-user
  silence stack build --allow-different-user

  if [ "$1" = test ]; then
    stack test
  else
    stack exec --allow-different-user -- melkor "$@"
  fi
}
