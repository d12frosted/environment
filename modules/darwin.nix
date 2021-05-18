{ config, lib, pkgs, ... }:

let
  fullName       = "Boris Buliga";
  home           = builtins.getEnv "HOME";
  xdg_configHome = "${home}/.config";
in {
  time.timeZone = "Europe/Kiev";

  nix = {
    package = pkgs.nixFlakes;

    extraOptions = ''
experimental-features = nix-command flakes
    '';
    maxJobs = "auto";
    buildCores = 0;
    trustedUsers = [ "root" "d12frosted" ];

    binaryCaches = [
      # Nightly Emacs build cache for github.com/cmacrae/emacs
      "https://cachix.org/api/v1/cache/emacs"
      "https://cachix.org/api/v1/cache/nix-community"
      "https://cachix.org/api/v1/cache/deploy-rs"
    ];

    binaryCachePublicKeys = [
      "emacs.cachix.org-1:b1SMJNLY/mZF6GxQE+eDBeps7WnkT0Po55TAyzwOxTY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];

    trustedBinaryCaches = config.nix.binaryCaches;

    nixPath = lib.mkForce [{
      ssh-config-file = "${xdg_configHome}/.ssh/config";
      ssh-auth-sock   = "${xdg_configHome}/gnupg/S.gpg-agent.ssh";
    }];
  };

  nixpkgs = {
    config.allowUnfree = true;
    overlays = [
      (import ../overlays)
    ];
  };

  environment = {
    shells = [
      pkgs.fish
      pkgs.zsh
      pkgs.bash
    ];
  };

  home-manager = {
    useGlobalPkgs = true;
    users.d12frosted = import <hm-config>;
  };

  system = {
    stateVersion = 4;

    defaults = {
      NSGlobalDomain = {
        AppleKeyboardUIMode = 3;
        ApplePressAndHoldEnabled = false;
        _HIHideMenuBar = false;
        "com.apple.keyboard.fnState" = true;
        "com.apple.mouse.tapBehavior" = 1;
        "com.apple.sound.beep.volume" = "0.0";
        "com.apple.sound.beep.feedback" = 0;
      };

      dock = {
        autohide = true;
        launchanim = false;
        orientation = "left";
      };
    };
  };

  users = {
    users.d12frosted = {
     shell = pkgs.fish;
     home = "/Users/d12frosted";
    };
    nix = {
      configureBuildUsers = true;
    };
  };

  programs = {
    fish.enable = true;
  };

  services = {
    nix-daemon.enable = true;
    activate-system.enable = true;
    skhd = {
      enable = true;
      package = pkgs.skhd;
      skhdConfig = ''
################################################################################
#
# window manipulation
#

alt - j : yabai -m query --spaces \
  | jq -re ".[] | select(.visible == 1).index" \
  | xargs -I{} yabai -m query --windows --space {} \
  | jq -sre 'add | map(select(.split != "none")) | sort_by(.display, .frame.x, .frame.y, .id) | reverse | nth(index(map(select(.focused == 1))) - 1).id' \
  | xargs -I{} yabai -m window --focus {}

alt - k : yabai -m query --spaces \
  | jq -re ".[] | select(.visible == 1).index" \
  | xargs -I{} yabai -m query --windows --space {} \
  | jq -sre 'add | map(select(.split != "none")) | sort_by(.display, .frame.x, .frame.y, .id) | nth(index(map(select(.focused == 1))) - 1).id' \
  | xargs -I{} yabai -m window --focus {}

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
    yabai = {
      package = pkgs.yabai;
      enable = true;
      enableScriptingAddition = true;

      config = {
        focus_follows_mouse          = "off";
        mouse_follows_focus          = "off";
        window_placement             = "second_child";
        window_topmost               = "on";
        window_opacity               = "off";
        window_opacity_duration      = "0.0";
        window_border                = "off";
        window_border_placement      = "inset";
        window_border_width          = 4;
        window_border_radius         = 0;
        active_window_border_topmost = "off";
        window_shadow                = "off";
        active_window_border_color   = "0xffFFBF46";
        normal_window_border_color   = "0xffE4FDE1";
        insert_window_border_color   = "0xffd75f5f";
        active_window_opacity        = "1.0";
        normal_window_opacity        = "1.0";
        split_ratio                  = "0.50";
        auto_balance                 = "off";
        mouse_modifier               = "fn";
        mouse_action1                = "move";
        mouse_action2                = "resize";
        layout                       = "bsp";
        top_padding                  = 8;
        bottom_padding               = 8;
        left_padding                 = 8;
        right_padding                = 8;
        window_gap                   = 8;
      };

      extraConfig = ''
# load scripting additions
sudo yabai --load-sa
yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"

# setup spaces
function setup_space {
  local idx="$1"
  local name="$2"
  yabai -m space --focus "$idx" || yabai -m space --create
  yabai -m space "$idx" --label "$name"
}
setup_space 1 main
setup_space 2 code
setup_space 3 web
setup_space 4 chat
setup_space 5 media
setup_space 6 other
yabai -m space --focus 1

# floating apps and windows
yabai -m rule --add app="^System Preferences$" manage=off
yabai -m rule --add app="^Cryptomator$" manage=off
      '';
    };
  };

}
