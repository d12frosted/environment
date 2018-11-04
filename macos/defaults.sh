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
# Setup keyboard
#

# Disable press-and-hold for keys in favor of key repeat.
defaults write -g ApplePressAndHoldEnabled -bool false

# Disable smart quotes as they’re annoying when typing code.
defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false

# Disable smart dashes as they’re annoying when typing code.
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false

# Disable auto-correct.
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false

#
# Setup Trackpad
#

# Enable tap to click. (Don't have to press down on the trackpad -- just tap it.)
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults write com.apple.AppleMultitouchTrackpad Clicking -bool true

# Disable three finger vertical swipe gesture
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadThreeFingerVertSwipeGesture 0
defaults write com.apple.AppleMultitouchTrackpad TrackpadThreeFingerVertSwipeGesture 0

# Enable secondary click
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadRightClick -bool true
defaults write com.apple.AppleMultitouchTrackpad TrackpadRightClick -bool true

# Disable four fingers vertical swipe
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadFourFingerVertSwipeGesture 0
defaults write com.apple.AppleMultitouchTrackpad TrackpadFourFingerVertSwipeGesture 0

# Swipe full-screen apps with four fingers
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadFourFingerHorizSwipeGesture 2
defaults write com.apple.AppleMultitouchTrackpad TrackpadFourFingerHorizSwipeGesture 2

# Enable three finger drag
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadThreeFingerDrag -bool true
defaults write com.apple.AppleMultitouchTrackpad TrackpadThreeFingerDrag -bool true

#
# Finder
#

# Always open everything in Finder's list view.
defaults write com.apple.Finder FXPreferredViewStyle Nlsv

# Finder: show all filename extensions.
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

# Disable desktop.
defaults write com.apple.finder CreateDesktop -bool false

# Show the ~/Library folder.
chflags nohidden ~/Library

# Show the /Volumes folder.
sudo chflags nohidden /Volumes

#
# Terminal
#

source "terminal.sh"

#
# Other
#

# Disable spaces rearrangement.
defaults write com.apple.dock mru-spaces -bool false

# Auto-hide menu bar.
defaults write NSGlobalDomain _HIHideMenuBar -bool true

# Use AirDrop over every interface. srsly this should be a default.
defaults write com.apple.NetworkBrowser BrowseAllInterfaces 1

#
# Kill affected applications
#

for app in "Activity Monitor" "Dock" "Finder" "Safari" "SystemUIServer" "Terminal" "cfprefsd";
do
  echo "Killing $app"
  killall "${app}"
done

echo "Done. Note that some of these changes require a logout/restart to take effect."
