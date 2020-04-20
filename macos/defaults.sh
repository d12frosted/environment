#!/usr/bin/env bash

if [ "$(uname -s)" != "Darwin" ]; then
  echo "Nothing to do on your system."
  exit 0
fi

# Keep-alive: update existing `sudo` time stamp until we finish
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

#
# Setup dock.
#

defaults write com.apple.dock orientation left
defaults write com.apple.dock show-recents -bool false
defaults write com.apple.dock tilesize -int 35
defaults write com.apple.dock minimize-to-application -bool true
defaults write com.apple.dock autohide -bool true
defaults write com.apple.dock static-only -bool true

#
# Finder
#

# Always open everything in Finder's list view.
defaults write com.apple.Finder FXPreferredViewStyle Nlsv

# Finder: show all filename extensions.
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

# Enable desktop.
defaults write com.apple.finder CreateDesktop -bool true

# Show the ~/Library folder.
chflags nohidden ~/Library

# Show the /Volumes folder.
sudo chflags nohidden /Volumes

#
# Terminal
#

# source "terminal.sh"

#
# Kill affected applications
#

for app in "Activity Monitor" "Dock" "Finder" "Safari" "SystemUIServer" "Terminal" "cfprefsd";
do
  echo "Killing $app"
  killall "${app}"
done

echo "Done. Note that some of these changes require a logout/restart to take effect."
