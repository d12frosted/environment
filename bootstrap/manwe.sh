#!/usr/bin/env bash

if [[ "$(uname)" != "Darwin" ]]; then
    echo "Unsupported operating system: $(uname). This script works on Darwin only."
    exit 1
fi

function check() {
    command -v "$1" >/dev/null 2>&1
}

check brew || {
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    brew update
}

check stack || {
    brew install haskell-stack
}
