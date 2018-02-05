#!/usr/bin/env bash

set -e

target=$XDG_CONFIG_HOME
if [[ "$target" = "" ]]; then
  target="$HOME/.config"
fi

if [[ -d "$target" ]]; then
  echo "$target already exists"
else
  git clone "git@github.com:d12frosted/environment.git" "$target"
fi


cd "$target" && {
  bash "bootstrap/manwe.sh"
}

cd "$target/bootstrap/melkor" && {
  stack clean
  stack build --pedantic
  stack exec melkor
}
