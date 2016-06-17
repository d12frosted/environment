#!/bin/sh

# setup global gitignore
git config --global core.excludesfile ~/.gitignore_global

# setup user info
git config --global user.useconfigonly true

# setup editor
git config --global core.editor emacsclient

# setup push method
git config --global push.default simple

# force signing
git config --global commit.gpgsign true

#
# aliases
#

# pretty lg alias
git config --global alias.lg "log --graph --pretty=format:'%Cred%h%Creset %C(bold blue)<%an> -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset'"

# push all commits to current branch and push tags to all remotes
git config --global alias.pushall '!for r in $(git remote); do git push $r $(git symbolic-ref --short HEAD); done && for r in $(git remote); do git push $r --tags; done'
