#!/usr/bin/env bash
#
# macOS Defaults
# Configure macOS system preferences via defaults command
#

set -euo pipefail

echo "Configuring macOS defaults..."

#
# General UI/UX
#

# Disable the sound effects on boot
sudo nvram SystemAudioVolume=" "

# Expand save panel by default
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true

# Expand print panel by default
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true

# Save to disk (not to iCloud) by default
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false

# Disable automatic termination of inactive apps
defaults write NSGlobalDomain NSDisableAutomaticTermination -bool true

# Near-instant dialog/sheet resize animations
defaults write NSGlobalDomain NSWindowResizeTime -float 0.001

# Reveal IP address, hostname, OS version, etc. when clicking the clock in the login window
sudo defaults write /Library/Preferences/com.apple.loginwindow AdminHostInfo HostName

#
# Keyboard & Input
#

# Set a blazingly fast keyboard repeat rate
defaults write NSGlobalDomain KeyRepeat -int 2
defaults write NSGlobalDomain InitialKeyRepeat -int 15

# Disable press-and-hold for keys in favor of key repeat
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

# Use F1, F2, etc. as standard function keys (hold Fn for brightness/media)
defaults write NSGlobalDomain com.apple.keyboard.fnState -bool true

# Disable auto-correct
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false

# Disable smart quotes, smart dashes, auto-capitalization and period
# substitution — they mangle code in native text fields
defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false
defaults write NSGlobalDomain NSAutomaticCapitalizationEnabled -bool false
defaults write NSGlobalDomain NSAutomaticPeriodSubstitutionEnabled -bool false

# Full keyboard access: Tab moves focus through all controls in dialogs
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

# Map Caps Lock to Control on every connected keyboard. The mapping is stored
# per keyboard (vendor-product pair, decimal), and takes effect on next login;
# run `hidutil property --set '{"UserKeyMapping":[{"HIDKeyboardModifierMappingSrc":0x700000039,"HIDKeyboardModifierMappingDst":0x7000000E0}]}'`
# to apply immediately without logging out.
caps_lock=30064771129 # HID usage 0x700000039
left_ctrl=30064771296 # HID usage 0x7000000E0
while read -r vid pid; do
  defaults -currentHost write -g \
    "com.apple.keyboard.modifiermapping.$((vid))-$((pid))-0" -array \
    "<dict><key>HIDKeyboardModifierMappingSrc</key><integer>${caps_lock}</integer><key>HIDKeyboardModifierMappingDst</key><integer>${left_ctrl}</integer></dict>"
done < <(hidutil list --matching '{"DeviceUsagePage":1,"DeviceUsage":6}' | awk '$1 ~ /^0x/ {print $1, $2}' | sort -u)

#
# System Keybindings
#
# Symbolic hotkey IDs: 60 = select previous input source, 61 = select next
# source in input menu, 64 = show Spotlight search, 65 = show Finder search
# window. Modifier masks: Shift=131072, Ctrl=262144, Option=524288,
# Cmd=1048576. Parameters are [ascii char (65535 = none), key code, modifiers].
#

# Disable Spotlight shortcuts (Cmd+Space and Cmd+Option+Space); Raycast is used instead
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 64 \
  "<dict><key>enabled</key><false/></dict>"
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 65 \
  "<dict><key>enabled</key><false/></dict>"

# Switch input source (language) with Option+Esc
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 60 \
  "<dict><key>enabled</key><true/><key>value</key><dict><key>type</key><string>standard</string><key>parameters</key><array><integer>65535</integer><integer>53</integer><integer>524288</integer></array></dict></dict>"

# Make the system pick up symbolic hotkey changes without re-login
/System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u

# Open Raycast with Cmd+Shift+Space (49 = Space key code)
defaults write com.raycast.macos raycastGlobalHotkey -string "Command-Shift-49"

#
# Trackpad
#

# Enable tap to click for this user and for the login screen
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1

# Trackpad: map bottom right corner to right-click
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadCornerSecondaryClick -int 2
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadRightClick -bool true
defaults -currentHost write NSGlobalDomain com.apple.trackpad.trackpadCornerClickBehavior -int 1
defaults -currentHost write NSGlobalDomain com.apple.trackpad.enableSecondaryClick -bool true

#
# Finder
#

# Allow quitting Finder via Cmd+Q
defaults write com.apple.finder QuitMenuItem -bool true

# Show full POSIX path in Finder window titles
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

# New Finder windows open in the home directory instead of Recents
defaults write com.apple.finder NewWindowTarget -string "PfHm"

# Hide hidden files by default
defaults write com.apple.finder AppleShowAllFiles -bool false

# Show all filename extensions
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

# Show status bar
defaults write com.apple.finder ShowStatusBar -bool true

# Show path bar
defaults write com.apple.finder ShowPathbar -bool true

# Keep folders on top when sorting by name
defaults write com.apple.finder _FXSortFoldersFirst -bool true

# When performing a search, search the current folder by default
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

# Disable the warning when changing a file extension
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

# Avoid creating .DS_Store files on network or USB volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true

# Use list view in all Finder windows by default
# Four-letter codes for the other view modes: `icnv`, `clmv`, `glyv`
defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"

# Disable the warning before emptying the Trash
defaults write com.apple.finder WarnOnEmptyTrash -bool false

# Show the ~/Library folder
chflags nohidden ~/Library

# Show the /Volumes folder
sudo chflags nohidden /Volumes

#
# Dock
#

# Position the Dock on the left side of the screen
defaults write com.apple.dock orientation -string "left"

# Set the icon size of Dock items to 36 pixels
defaults write com.apple.dock tilesize -int 36

# Minimize windows into their application's icon
defaults write com.apple.dock minimize-to-application -bool true

# Show indicator lights for open applications in the Dock
defaults write com.apple.dock show-process-indicators -bool true

# Don't animate opening applications from the Dock
defaults write com.apple.dock launchanim -bool false

# Speed up Mission Control animations
defaults write com.apple.dock expose-animation-duration -float 0.1

# Don't automatically rearrange Spaces based on most recent use
defaults write com.apple.dock mru-spaces -bool false

# Remove the auto-hiding Dock delay
defaults write com.apple.dock autohide-delay -float 0

# Remove the animation when hiding/showing the Dock
defaults write com.apple.dock autohide-time-modifier -float 0

# Automatically hide and show the Dock
defaults write com.apple.dock autohide -bool true

# Make Dock icons of hidden applications translucent
defaults write com.apple.dock showhidden -bool true

# Don't show recent applications in Dock
defaults write com.apple.dock show-recents -bool false

#
# Safari & WebKit
#
# Safari is sandboxed, so writing its preferences requires the terminal to
# have Full Disk Access (System Settings -> Privacy & Security -> Full Disk
# Access). Reading them has the same requirement, which makes it a reliable
# probe: skip the section with a warning instead of aborting the whole run.

if defaults read com.apple.Safari &> /dev/null; then
  # Privacy: don't send search queries to Apple
  defaults write com.apple.Safari UniversalSearchEnabled -bool false
  defaults write com.apple.Safari SuppressSearchSuggestions -bool true

  # Show the full URL in the address bar (note: this still hides the scheme)
  defaults write com.apple.Safari ShowFullURLInSmartSearchField -bool true

  # Enable the Develop menu and the Web Inspector in Safari
  defaults write com.apple.Safari IncludeDevelopMenu -bool true
  defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey -bool true
  defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled -bool true

  # Enable "Do Not Track"
  defaults write com.apple.Safari SendDoNotTrackHTTPHeader -bool true
else
  echo "⚠ Skipping Safari defaults: grant Full Disk Access to your terminal and re-run 'eru install macos'"
fi

#
# Terminal
#

# Only use UTF-8 in Terminal.app
defaults write com.apple.terminal StringEncodings -array 4

# Disable the annoying line marks
defaults write com.apple.Terminal ShowLineMarks -int 0

#
# Activity Monitor
#

# Show the main window when launching Activity Monitor
defaults write com.apple.ActivityMonitor OpenMainWindow -bool true

# Visualize CPU usage in the Activity Monitor Dock icon
defaults write com.apple.ActivityMonitor IconType -int 5

# Show all processes in Activity Monitor
defaults write com.apple.ActivityMonitor ShowCategory -int 0

# Sort Activity Monitor results by CPU usage
defaults write com.apple.ActivityMonitor SortColumn -string "CPUUsage"
defaults write com.apple.ActivityMonitor SortDirection -int 0

#
# TextEdit
#

# Use plain text mode for new TextEdit documents
defaults write com.apple.TextEdit RichText -int 0

# Open and save files as UTF-8 in TextEdit
defaults write com.apple.TextEdit PlainTextEncoding -int 4
defaults write com.apple.TextEdit PlainTextEncodingForWrite -int 4

#
# Screenshots
#

# Save screenshots to ~/Pictures/Screenshots
mkdir -p "${HOME}/Pictures/Screenshots"
defaults write com.apple.screencapture location -string "${HOME}/Pictures/Screenshots"

# Save screenshots in PNG format (other options: BMP, GIF, JPG, PDF, TIFF)
defaults write com.apple.screencapture type -string "png"

# Disable shadow in screenshots
defaults write com.apple.screencapture disable-shadow -bool true

#
# Apply changes
#

echo "Done. Note that some of these changes require a logout/restart to take effect."

# Kill affected applications
for app in "Activity Monitor" \
  "Dock" \
  "Finder" \
  "Safari" \
  "SystemUIServer" \
  "Terminal"; do
  killall "${app}" &> /dev/null || true
done
