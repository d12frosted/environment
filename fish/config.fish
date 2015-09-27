# variables

set -x PATH $HOME/.local/bin /usr/texbin $PATH
set -x EDITOR "emacsclient"
set fish_greeting ""

alias ghc "stack ghc"
alias ghci "stack ghci"
alias cabal "stack cabal"
alias runghc "stack runghc"
alias runhaskell "stack runghc"
alias haddock "stack haddock"

# theme
set fish_color_command blue
set fish_color_param "715ab1"
set fish_color_autosuggestion "9f8fbd"
set fish_color_operator "428374"
set fish_color_end "8959a8"
set fish_color_error "e0211d"
set fish_color_comment "2aa198"
set fish_color_quote "718c00"
set fish_color_redirection "3e999f"
