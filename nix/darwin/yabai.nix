{ config, pkgs, lib, ... }:
let
  jq = "${pkgs.jq}/bin/jq";
  open_alacritty = "open -na ${pkgs.alacritty}/Applications/Alacritty.app";
  set_shell = "export SHELL=/run/current-system/sw/bin/fish";
  open_emacs = ''open -na "$(brew --cellar emacs-plus@30)/30.1/Emacs.app"'';
in {

  home.file.skhd = {
    target = ".config/skhd/skhdrc";
    text = ''
      ################################################################################
      #
      # window manipulation
      #

      lalt - return : yabai -m window --swap first
      lalt + shift - space : yabai -m space --balance
      lalt - space : yabai-layout-toggle

      alt - r : yabai -m space --rotate 90

      lalt - j : yabai-window-focus prev
      lalt - k : yabai-window-focus next

      lalt - h : yabai -m window --resize left:-100:0 || yabai -m window --resize right:-100:0
      lalt - l : yabai -m window --resize right:100:0 || yabai -m window --resize left:100:0
      lalt + shift - h : yabai -m window --resize bottom:0:100 || yabai -m window --resize top:0:100
      lalt + shift - l : yabai -m window --resize top:0:-100 || yabai -m window --resize bottom:0:-100

      ################################################################################
      #
      # space manipulation
      #

      lalt - 1 : yabai -m space --focus 1
      lalt - 2 : yabai -m space --focus 2
      lalt - 3 : yabai -m space --focus 3
      lalt - 4 : yabai -m space --focus 4
      lalt - 5 : yabai -m space --focus 5
      lalt - 6 : yabai -m space --focus 6

      lalt + shift - 1 : yabai -m window --space 1; yabai -m space --focus 1
      lalt + shift - 2 : yabai -m window --space 2; yabai -m space --focus 2
      lalt + shift - 3 : yabai -m window --space 3; yabai -m space --focus 3
      lalt + shift - 4 : yabai -m window --space 4; yabai -m space --focus 4
      lalt + shift - 5 : yabai -m window --space 5; yabai -m space --focus 5
      lalt + shift - 6 : yabai -m window --space 6; yabai -m space --focus 6

      ################################################################################
      #
      # Applications
      #

      lalt + shift - c [
        "emacs" : skhd -k "ctrl - x" ; skhd -k "ctrl - c"
        *       : skhd -k "cmd - q"
      ]

      ################################################################################
      #
      # Mode for opening applications
      #

      :: open @
      lalt - o ; open
      open < lalt - o ; default

      # emacs
      open < e : ${set_shell} ; ${open_emacs} ; skhd -k "alt - o"
      open < shift - e : nohup emacs --debug-init &>/dev/null & ; skhd -k "alt - o"

      # alacritty
      open < return : ${open_alacritty} ; skhd -k "alt - o"
      alt + shift - return : ${open_alacritty}

      ################################################################################
      #
      # Blacklist some applications
      #

      .blacklist [
        "Final Fantasy XIV"
        "ffxiv_dx11.exe"
      ]

      lalt - f : terminal-notifier -message $SHELL -group system-wide-whisper -timeout 1
    '';
  };
}
