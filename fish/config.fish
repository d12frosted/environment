# variables

set -x PATH $HOME/.cabal/bin /usr/local/bin /bin /usr/sbin /sbin /usr/bin $HOME/.bin $HOME/.rvm/bin
set -x EDITOR "emacs"
set fish_greeting

# git

git config --global alias.lg "log --graph --pretty=format:'%Cred%h%Creset %C(bold blue)<%an> -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset'"

# functions

function _git_branch_name
  echo (command git symbolic-ref HEAD ^/dev/null | sed -e 's|^refs/heads/||')
end

function _git_is_dirty
  echo (command git status -s --ignore-submodules=dirty ^/dev/null)
end

function _git_is_cherry
  set -l cherry_count (count (command git cherry -v ^ /dev/null))
  if test $cherry_count -ne 0
    echo $cherry_count
  else
    echo ''
  end
end

function e
  emacs $argv
end

function ee
  perl ~/.environment/emacs_starter.pl $argv
end
