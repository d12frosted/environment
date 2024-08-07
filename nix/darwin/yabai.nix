{ config, pkgs, lib, ... }: let
  jq = "${pkgs.jq}/bin/jq";
in {
  home.file.yabai = {
    executable = true;
    target = ".config/yabai/yabairc";
    text = ''
#!/usr/bin/env sh

# load scripting additions
yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"
sudo yabai --load-sa

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

yabai -m config insert_feedback_color 0xffd75f5f
yabai -m config active_window_border_color 0xff37474F
yabai -m config normal_window_border_color 0xffECEFF1
yabai -m config window_border_width 2
yabai -m config window_border_radius 0
yabai -m config window_border_blur off
yabai -m config window_border_hidpi on
yabai -m config window_border off

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
yabai -m rule --add app="^Calculator$" manage=off
yabai -m rule --add app="^Archive Utility$" manage=off
yabai -m rule --add app="^Cryptomator$" manage=off
yabai -m rule --add app="^NIIMBOT$" manage=off
yabai -m rule --add app="^Emacs$" title!='^$' manage=on
yabai -m rule --add app="^XIV on Mac$" manage=off
yabai -m rule --add app="^Finder$" title="Copy" manage=off
yabai -m rule --add title="^Preferences" manage=off
yabai -m rule --add title="^Settings" manage=off

# move some apps automatically to specific spaces
yabai -m rule --add app="^Safari$" space=3
yabai -m rule --add app="^Firefox$" space=3
yabai -m rule --add app="^Arc$" space=3
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

alt - j : yabai-window-focus prev
alt - k : yabai-window-focus next

# shif̋t + alt - j : yabai -m window --resize left:-20:0
# shift + alt - k : yabai -m window --resize right:-20:0

alt - space : yabai-layout-toggle

################################################################################
#
# space manipulation
#

alt - f1 : yabai -m space --focus 1
alt - f2 : yabai -m space --focus 2
alt - f3 : yabai -m space --focus 3
alt - f4 : yabai -m space --focus 4
alt - f5 : yabai -m space --focus 5
alt - f6 : yabai -m space --focus 6

shift + alt - f1 : yabai -m window --space 1; yabai -m space --focus 1
shift + alt - f2 : yabai -m window --space 2; yabai -m space --focus 2
shift + alt - f3 : yabai -m window --space 3; yabai -m space --focus 3
shift + alt - f4 : yabai -m window --space 4; yabai -m space --focus 4
shift + alt - f5 : yabai -m window --space 5; yabai -m space --focus 5
shift + alt - f6 : yabai -m window --space 6; yabai -m space --focus 6

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
