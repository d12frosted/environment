#!/bin/bash

/usr/bin/osascript -e 'tell application "System Events"' -e 'set frontApp to name of first application process whose frontmost is true' -e 'end tell'
