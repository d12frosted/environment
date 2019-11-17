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

check() {
  command -v "$1" >/dev/null 2>&1
}

#
# Haskell is the language of Eru.
#

if check stack; then
  stack upgrade
else
  curl -sSL https://get.haskellstack.org/ | sh
fi

stack install shake
