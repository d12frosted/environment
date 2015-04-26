#!/bin/sh

# setup global gitignore
git config --global core.excludesfile ~/.gitignore_global

# setup user info
git config --global user.name "d12frosted"
git config --global user.email d12frosted@icloud.com

# setup editor
git config --global core.editor emacsclient

# pretty lg alias
git config --global alias.lg "log --graph --pretty=format:'%Cred%h%Creset %C(bold blue)<%an> -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset'"

# setup push method
git config --global push.default simple
