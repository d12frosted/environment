# Only use UTF-8 in Terminal.
# defaults write -app Terminal StringEncodings -array 4

# Copy "Basic" profile to "d12frosted", and configure "d12frosted" the way we want it.
defaults+ copy com.apple.Terminal 'Window Settings.Basic' 'Window Settings.d12frosted'
defaults write com.apple.Terminal 'Default Window Settings' 'd12frosted'
defaults write com.apple.Terminal 'Startup Window Settings' 'd12frosted'
plutil -replace 'Window Settings.d12frosted.Font' -data 'YnBsaXN0MDDUAQIDBAUGGBlYJHZlcnNpb25YJG9iamVjdHNZJGFyY2hpdmVyVCR0b3ASAAGGoKQH
CBESVSRudWxs1AkKCwwNDg8QVk5TU2l6ZVhOU2ZGbGFnc1ZOU05hbWVWJGNsYXNzI0AmAAAAAAAA
EBCAAoADXxAVU291cmNlQ29kZVByby1SZWd1bGFy0hMUFRZaJGNsYXNzbmFtZVgkY2xhc3Nlc1ZO
U0ZvbnSiFRdYTlNPYmplY3RfEA9OU0tleWVkQXJjaGl2ZXLRGhtUcm9vdIABCBEaIy0yNzxCS1Jb
YmlydHZ4kJWgqbCzvM7R1gAAAAAAAAEBAAAAAAAAABwAAAAAAAAAAAAAAAAAAADY' -- ~/Library/Preferences/com.apple.Terminal.plist
defaults+ write com.apple.Terminal 'Window Settings.d12frosted.name' 'd12frosted'
defaults+ write com.apple.Terminal 'Window Settings.d12frosted.CursorBlink' 0
defaults+ write com.apple.Terminal 'Window Settings.d12frosted.Bell' 0
defaults+ write com.apple.Terminal 'Window Settings.d12frosted.VisualBell' 1
defaults+ write com.apple.Terminal 'Window Settings.d12frosted.useOptionAsMetaKey' 1
defaults+ write com.apple.Terminal 'Window Settings.d12frosted.CommandString' 'fish'
defaults+ write com.apple.Terminal 'Window Settings.d12frosted.RunCommandAsShell' 0

# Copy "d12frosted" profile to "d12frosted Presentations", and change font and window sizes.
defaults+ copy com.apple.Terminal 'Window Settings.Basic' 'Window Settings.d12frosted-presentation'
defaults+ write com.apple.Terminal 'Window Settings.d12frosted-presentation.name' 'd12frosted-presentation'

killall Terminal
killall cfprefsd
