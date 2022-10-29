{ config, lib, pkgs, ... }:

# References:
#
# https://daiderd.com/nix-darwin/manual/index.html#sec-options
# https://github.com/jwiegley/nix-config/blob/master/config/darwin.nix
# https://github.com/cmacrae/config/blob/master/modules/macintosh.nix

let
  fullName       = "Boris Buliga";
  user           = builtins.getEnv "USER";
  home           = builtins.getEnv "HOME";
  xdg_configHome = "${home}/.config";
in {
  time.timeZone = "Europe/Kiev";

  nix = {
    package = pkgs.nixFlakes;

    extraOptions = ''
experimental-features = nix-command flakes
    '';

    configureBuildUsers = true;
    settings = {
      max-jobs = "auto";
      cores = 0;

      trusted-users = [ "root" "d12frosted" ];

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
    overlays = [
      (import ./overlays)
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
    users.d12frosted = lib.mkMerge [
      (import ./home.nix)
      {
        imports = [
          ./darwin/yabai.nix
        ];
      }
    ];
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

      finder = {
        AppleShowAllExtensions = true;
      };

      trackpad = {
        Clicking = true;
      };
    };

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
  };

  networking.hostName = "d12frosted";
  users = {
    users.d12frosted = {
     shell = pkgs.fish;
     home = "/Users/d12frosted";
    };
  };

  programs.fish.enable = true;

  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.enableSSHSupport = true;

  services = {
    nix-daemon.enable = true;
    activate-system.enable = true;
  };

  launchd.user.agents.vulpea-sync = {
    command = "${xdg_configHome}/bin/vulpea-sync";
    environment = {
      VULPEA_DIR = "${home}/Dropbox/vulpea";
    };
    path = [
      pkgs.bash
      pkgs.git
      pkgs.git-lfs
      pkgs.coreutils
      pkgs.openssh
    ];
    serviceConfig = {
      StartInterval = 60;
      StandardErrorPath = "/Users/d12frosted/vulpea-sync.log";
      StandardOutPath = "/Users/d12frosted/vulpea-sync.log";
      RunAtLoad = true;
    };
  };

  homebrew = {
    enable = true;
    onActivation.autoUpdate = true;
    onActivation.cleanup = "uninstall"; # removes manually install brews and casks
    brews = [
      "llvm"
      {
        name = "emacs-plus@29";
        args = ["with-dragon-icon"];
        link = true;
      }
      {
        name = "yabai";
        args = [];
        restart_service = true;
      }
      {
        name = "skhd";
        args = [];
        restart_service = true;
      }
    ];
    casks = [
      # multimedia
      "spotify"
      "vlc"

      # extra browsers
      "firefox"
      "tor-browser"

      # social
      "telegram"

      # network
      "protonvpn"
      "transmission"

      # system tweaks
      "raycast"
      "karabiner-elements"
      "flameshot"

      # other
      "appcleaner"
    ];
    taps = [
      "d12frosted/emacs-plus"
      "homebrew/bundle"
      "homebrew/cask"
      "homebrew/cask"
      "homebrew/cask-drivers"
      "homebrew/core"
      "homebrew/services"
      "koekeishiya/formulae"
    ];
  };

}
