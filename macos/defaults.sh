#!/usr/bin/env bash

if [ "$(uname -s)" != "Darwin" ]; then
  echo "Nothing to do on your system."
  exit 0
fi

# Ask for the administrator password upfront
sudo -v

# Keep-alive: update existing `sudo` time stamp until we finish
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

# Setup dock.
defaults write com.apple.dock orientation left
defaults write com.apple.dock show-recents -bool false
defaults write com.apple.dock tilesize -int 35
defaults write com.apple.dock minimize-to-application -bool true
defaults write com.apple.dock autohide -bool true

# Disable spaces rearrangement.
defaults write com.apple.dock mru-spaces -bool false

# Auto-hide menu bar.
defaults write NSGlobalDomain _HIHideMenuBar -bool true

# Disable press-and-hold for keys in favor of key repeat.
defaults write -g ApplePressAndHoldEnabled -bool false

# Use AirDrop over every interface. srsly this should be a default.
defaults write com.apple.NetworkBrowser BrowseAllInterfaces 1

# Always open everything in Finder's list view.
defaults write com.apple.Finder FXPreferredViewStyle Nlsv

# Finder: show all filename extensions.
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

# Show the ~/Library folder.
chflags nohidden ~/Library

# Show the /Volumes folder.
sudo chflags nohidden /Volumes

# Disable smart quotes as they’re annoying when typing code.
defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false

# Disable smart dashes as they’re annoying when typing code.
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false

# Disable auto-correct.
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false

# Kill affected applications
for app in "Activity Monitor" "Dock" "Finder" "Safari" "SystemUIServer" "cfprefsd" "Terminal";
do
  echo "Killing $app"
  killall "${app}" &> /dev/null || true
done

echo "Done. Note that some of these changes require a logout/restart to take effect."
