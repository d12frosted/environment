#!/usr/bin/env bash

set -e

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


cd "$target" && {
  bash "bootstrap/manwe.sh"
}

# cd "$target/bootstrap/melkor" && {
#   stack clean
#   stack build --pedantic
#   stack exec melkor
# }
