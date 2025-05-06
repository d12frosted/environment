{ config, lib, pkgs, ... }:

# References:
#
# https://daiderd.com/nix-darwin/manual/index.html#sec-options
# https://github.com/jwiegley/nix-config/blob/master/config/darwin.nix
# https://github.com/cmacrae/config/blob/master/modules/macintosh.nix

let
  fullName = "Boris Buliga";
  user = builtins.getEnv "USER";
  home = builtins.getEnv "HOME";
  xdg_configHome = "${home}/.config";
in {
  time.timeZone = "Europe/Kiev";

  nix = {
    enable = true;
    package = pkgs.nixVersions.stable;

    extraOptions = ''
      experimental-features = nix-command flakes
    '';

    settings = {
      max-jobs = "auto";
      cores = 0;

      trusted-users = [ "root" "@wheel" user ];

      trusted-public-keys = [
        "emacs.cachix.org-1:b1SMJNLY/mZF6GxQE+eDBeps7WnkT0Po55TAyzwOxTY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      ];

      trusted-substituters = [
        "https://cachix.org/api/v1/cache/emacs"
        "https://cachix.org/api/v1/cache/nix-community"
        "https://cachix.org/api/v1/cache/deploy-rs"
        "https://hydra.iohk.io"
      ];
    };
  };

  nixpkgs = {
    config.allowUnfree = true;
    overlays = [ (import ./overlays) ];
  };

  environment = { shells = [ pkgs.fish pkgs.zsh pkgs.bash ]; };

  home-manager = {
    useGlobalPkgs = true;
    users = {
      "${user}" = lib.mkMerge [
        (import ./home.nix)
        { imports = [ ./darwin/yabai.nix ]; }
      ];
    };
  };

  system = {
    stateVersion = 4;

    defaults = {
      NSGlobalDomain = {
        AppleKeyboardUIMode = 3;
        ApplePressAndHoldEnabled = false;
        InitialKeyRepeat = 12;
        KeyRepeat = 2;
        AppleShowAllExtensions = true;
        NSAutomaticCapitalizationEnabled = false;
        NSAutomaticDashSubstitutionEnabled = false;
        NSAutomaticPeriodSubstitutionEnabled = false;
        NSAutomaticQuoteSubstitutionEnabled = false;
        NSAutomaticSpellingCorrectionEnabled = false;
        NSTableViewDefaultSizeMode = 2;
        _HIHideMenuBar = false;
        "com.apple.keyboard.fnState" = true;
        "com.apple.mouse.tapBehavior" = 1;
        "com.apple.sound.beep.volume" = 0.0;
        "com.apple.sound.beep.feedback" = 0;
      };

      dock = {
        autohide = true;
        launchanim = false;
        minimize-to-application = true;
        mru-spaces = false;
        orientation = "left";
        show-recents = false;
        static-only = true;
        tilesize = 32;
      };

      finder = { AppleShowAllExtensions = true; };

      trackpad = { Clicking = true; };
    };

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
  };

  networking.hostName = "${user}";
  users = {
    users = {
      "${user}" = {
        shell = pkgs.fish;
        home = "${home}";
      };
    };
  };

  programs.fish.enable = true;

  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.enableSSHSupport = true;

  launchd.user.agents.vulpea-sync = {
    command = "${xdg_configHome}/bin/vulpea-sync";
    environment = { VULPEA_DIR = "${home}/vulpea"; };
    path = [ pkgs.bash pkgs.git pkgs.git-lfs pkgs.coreutils pkgs.openssh ];
    serviceConfig = {
      StartInterval = 60;
      StandardErrorPath = "${home}/vulpea-sync.log";
      StandardOutPath = "${home}/vulpea-sync.log";
      RunAtLoad = true;
    };
  };
  homebrew = {
    enable = true;
    onActivation.autoUpdate = true;
    onActivation.cleanup =
      "uninstall"; # removes manually install brews and casks
    brews = [
      "llvm"
      "pandoc"
      "hq"
      "terminal-notifier"
      "enchant"
      {
        name = "emacs-plus@30";
        args = [ "with-dragon-icon" ];
        link = true;
      }
      {
        name = "yabai";
        args = [ ];
      }
      {
        name = "skhd";
        args = [ ];
      }
    ];
    casks = [
      # multimedia
      "spotify"
      "vlc"
      "mpv"
      "stolendata-mpv"
      "cog"

      # extra browsers
      "firefox"
      "tor-browser"
      "zen-browser"

      # social
      "telegram"
      "element"

      # network
      "protonvpn"
      "transmission"

      # other
      "appcleaner"
      "bartender"
      "chatgpt"
      "cryptomator"
      "dbeaver-community"
      "figma"
      "flameshot"
      "maccy"
      "proton-pass"
      "rancher"
      "raycast"
      "syncthing"
      "warp"
      "whisky"
      "workflowy"
    ];
    taps = [
      "d12frosted/emacs-plus"
      "homebrew/bundle"
      "homebrew/cask-drivers"
      "homebrew/services"
      "koekeishiya/formulae"
    ];
  };

}
