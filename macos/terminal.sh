#!/usr/bin/env bash

plist_file=$HOME/Library/Preferences/com.apple.Terminal.plist # $HOME/example.plist #
profile_name=d12frosted

function write() {
  plutil -replace "$1" "-$2" "$3" -- "$plist_file"
}

function write_bool() {
  write "$1" bool "$2"
}

function write_integer() {
  write "$1" integer "$2"
}

function write_float() {
  write "$1" float "$2"
}

function write_string() {
  write "$1" string "$2"
}

function write_date() {
  write "$1" date "$2"
}

function write_data() {
  write "$1" data "$2"
}

function write_xml() {
  write "$1" xml "$2"
}

function write_json() {
  write "$1" json "$2"
}

function insert() {
  plutil -insert "$1" -json "{}" -- "$plist_file" >/dev/null 2>&1
}

# Setup "$profile_name" profile
insert "Window Settings.$profile_name"
write_string "Window Settings.$profile_name.name" "$profile_name"
write_integer "Window Settings.$profile_name.Bell" 0
write_string "Window Settings.$profile_name.CommandString" "fish"
write_integer "Window Settings.$profile_name.CursorBlink" 0
write_data "Window Settings.$profile_name.Font" \
           "YnBsaXN0MDDUAQIDBAUGGBlYJHZlcnNpb25YJG9iamVjdHNZJGFyY2hpdmVyVCR0b3AS \
           AAGGoKQHCBESVSRudWxs1AkKCwwNDg8QVk5TU2l6ZVhOU2ZGbGFnc1ZOU05hbWVWJGNs \
           YXNzI0AmAAAAAAAAEBCAAoADXxAVU291cmNlQ29kZVByby1SZWd1bGFy0hMUFRZaJGNs \
           YXNzbmFtZVgkY2xhc3Nlc1ZOU0ZvbnSiFRdYTlNPYmplY3RfEA9OU0tleWVkQXJjaGl2 \
           ZXLRGhtUcm9vdIABCBEaIy0yNzxCS1JbYmlydHZ4kJWgqbCzvM7R1gAAAAAAAAEBAAAA \
           AAAAABwAAAAAAAAAAAAAAAAAAADY"
write_bool "Window Settings.$profile_name.FontAntialias" YES
write_float "Window Settings.$profile_name.FontWidthSpacing" 1.0
write_float "Window Settings.$profile_name.ProfileCurrentVersion" 2.0600000000000001
write_integer "Window Settings.$profile_name.RunCommandAsShell" 0
write_integer "Window Settings.$profile_name.VisualBell" 1
write_string "Window Settings.$profile_name.type" "Window Settings"
write_integer "Window Settings.$profile_name.useOptionAsMetaKey" 1

# Set "$profile_name" as default
write_string "Default Window Settings" "$profile_name"
write_string "Startup Window Settings" "$profile_name"
