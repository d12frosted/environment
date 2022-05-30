{ config, pkgs, lib, ... }: let
  jq = "${pkgs.jq}/bin/jq";
in {
  home.file.yabai = {
    executable = true;
    target = ".config/yabai/yabairc";
    text = ''
#!/usr/bin/env sh

# load scripting additions
sudo yabai --load-sa
yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"

# config
yabai -m config layout bsp
yabai -m config top_padding    8
yabai -m config bottom_padding 8
yabai -m config left_padding   8
yabai -m config right_padding  8
yabai -m config window_gap     8
yabai -m config auto_balance off
yabai -m config split_ratio 0.5
yabai -m config window_shadow off

#
# setup spaces
#
for _ in $(yabai -m query --spaces | jq '.[].index | select(. > 6)'); do
  yabai -m space --destroy 7
done

function setup_space {
  local idx="$1"
  local name="$2"
  local space=
  echo "setup space $idx : $name"

  space=$(yabai -m query --spaces --space "$idx")
  if [ -z "$space" ]; then
    yabai -m space --create
  fi

  yabai -m space "$idx" --label "$name"
}

setup_space 1 emacs
setup_space 2 code
setup_space 3 web
setup_space 4 social
setup_space 5 media
setup_space 6 other

# floating apps and windows
yabai -m rule --add app="^System Preferences$" manage=off
yabai -m rule --add app="^Cryptomator$" manage=off
yabai -m rule --add app="^Emacs$" title!='^$' manage=on

# move some apps automatically to specific spaces
yabai -m rule --add app="^Safari$" space=3
yabai -m rule --add app="^Firefox$" space=3
yabai -m rule --add app="^Telegram$" space=4
yabai -m rule --add app="^Messages$" space=4
yabai -m rule --add app="^Music$" space=5
yabai -m rule --add app="^Spotify$" space=5
yabai -m rule --add app="^Transmission$" space=6
      '';
  };

  home.file.skhd = {
    target = ".config/skhd/skhdrc";
    text = ''
################################################################################
#
# window manipulation
#

alt - j : yabai -m query --spaces \
  | ${jq} -re '.[] | select(."is-visible").index' \
  | xargs -I{} yabai -m query --windows --space {} \
  | ${jq} -sre 'add | map(select(."split-type" != "none")) | sort_by(.display, .frame.x, .frame.y, .id) | reverse | nth(index(map(select(."has-focus"))) - 1).id' \
  | xargs -I{} yabai -m window --focus {}

alt - k : yabai -m query --spaces \
  | ${jq} -re '.[] | select(."is-visible").index' \
  | xargs -I{} yabai -m query --windows --space {} \
  | ${jq} -sre 'add | map(select(."split-type" != "none")) | sort_by(.display, .frame.x, .frame.y, .id) | nth(index(map(select(."has-focus"))) - 1).id' \
  | xargs -I{} yabai -m window --focus {}

# shif̋t + alt - j : yabai -m window --resize left:-20:0
# shift + alt - k : yabai -m window --resize right:-20:0

alt - space : yabai -m window --toggle zoom-fullscreen

################################################################################
#
# space manipulation
#

alt - 1 : yabai -m space --focus 1
alt - 2 : yabai -m space --focus 2
alt - 3 : yabai -m space --focus 3
alt - 4 : yabai -m space --focus 4
alt - 5 : yabai -m space --focus 5
alt - 6 : yabai -m space --focus 6

shift + alt - 1 : yabai -m window --space 1; yabai -m space --focus 1
shift + alt - 2 : yabai -m window --space 2; yabai -m space --focus 2
shift + alt - 3 : yabai -m window --space 3; yabai -m space --focus 3
shift + alt - 4 : yabai -m window --space 4; yabai -m space --focus 4
shift + alt - 5 : yabai -m window --space 5; yabai -m space --focus 5
shift + alt - 6 : yabai -m window --space 6; yabai -m space --focus 6

################################################################################
#
# window manipulation
#

alt - return : yabai -m window --swap first

################################################################################
#
# Applications
#

shift + alt - c [
  "emacs" : skhd -k "ctrl - x" ; skhd -k "ctrl - c"
  *       : skhd -k "cmd - q"
]

################################################################################
#
# Mode for opening applications
#

:: open @
alt - o ; open
open < alt - o ; default

# emacs
open < e : nohup emacs &>/dev/null & ; skhd -k "alt - o"
open < shift - e : nohup emacs --debug-init &>/dev/null & ; skhd -k "alt - o"

# alacritty
open < return : open -na ${pkgs.alacritty}/Applications/Alacritty.app ; skhd -k "alt - o"
shift + alt - return : open -na ${pkgs.alacritty}/Applications/Alacritty.app
      '';
  };
}
