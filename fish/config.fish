# variables

set -x PATH $HOME/.cabal/bin /usr/local/bin /bin /usr/sbin /sbin /usr/bin $HOME/.bin $HOME/.rvm/bin
set -x EDITOR "emacsclient"
set fish_greeting

# git

git config --global alias.lg "log --graph --pretty=format:'%Cred%h%Creset %C(bold blue)<%an> -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset'"

# aliases

alias e "emacs -nw"
alias ee "perl ~/.environment/emacs_starter.pl"
alias marked "open -a '/Applications/Marked 2.app'"
