# variables

set -x PATH $HOME/.cabal/bin /usr/local/bin /bin /usr/sbin /sbin /usr/bin $HOME/.bin $HOME/.rvm/bin
set -x EDITOR "emacs"
set fish_greeting

# coloring

set -g black (set_color black)
set -g red (set_color red)
set -g green (set_color green)
set -g yellow (set_color yellow)
set -g blue (set_color blue)
set -g purple (set_color purple)
set -g cyan (set_color cyan)
set -g white (set_color white)
set -g normal (set_color normal)

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
