{ pkgs, lib, config, ... }:

# References:
#
# https://nix-community.github.io/home-manager/options.html
# https://github.com/jwiegley/nix-config/blob/master/config/home.nix
# https://github.com/cmacrae/config/blob/master/modules/macintosh.nix

let
  home            = builtins.getEnv "HOME";
  tmpdir          = "/tmp";
  emacs-server    = "${tmpdir}/emacs-emacs/server";
  emacsclient     = "${pkgs.emacs}/bin/emacsclient -s ${emacs-server}";
in {
  home = {
    # These are packages that should always be present in the user
    # environment, though perhaps not the machine environment.
    packages = pkgs.callPackage ./packages.nix {};

    sessionVariables = {
      ASPELL_CONF                = "dict-dir ${home}/.nix-profile/lib/aspell";
      BASE16_HOME                = "${pkgs.base16-shell}";
      CMD_NOTIFICATION_THRESHOLD = "8000";
      EDITOR                     = "${emacsclient}";
      EMACS_SERVER_FILE          = "${emacs-server}";
      GNUPGHOME                  = "${config.xdg.configHome}/gnupg";
      NIX_CONF                   = "${config.xdg.configHome}/nix";
      PROJECTS_HOME              = "${home}/Developer";
      SSH_AUTH_SOCK              = "${config.xdg.configHome}/gnupg/S.gpg-agent.ssh";
      XDG_CACHE_HOME             = config.xdg.cacheHome;
      XDG_CONFIG_HOME            = config.xdg.configHome;
      XDG_DATA_HOME              = config.xdg.dataHome;
    };
    sessionPath = [
      "${config.xdg.configHome}/bin"
      "${home}/.local/bin"
    ];
  };

  xdg = {
    enable = true;

    configFile."gnupg/gpg-agent.conf".text = ''
      enable-ssh-support
      default-cache-ttl 86400
      max-cache-ttl 86400
    '' + lib.optionalString pkgs.stdenv.isDarwin ''
      pinentry-program ${pkgs.pinentry_mac}/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac
    '' + lib.optionalString pkgs.stdenv.isLinux ''
       pinentry-program ${pkgs.pinentry-qt}
    '';
  };

  programs = {
    git = {
      enable = true;
      userName = "Boris Buliga";
      userEmail = "boris@d12frosted.io";
      aliases = {
        lg = "log --graph --pretty=format:'%Cred%h%Creset %C(bold blue)<%an> -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset'";
      };
      extraConfig = {
        magithub = {
          online = true;
        };
        github = {
          user = "d12frosted";
        };
      };
      signing = {
        key = "F9EBF09436BCB50F";
        signByDefault = true;
      };
    };

    gpg = {
      enable = true;
      homedir = "${config.xdg.configHome}/gnupg";
      settings = {
        default-key = "F9EBF09436BCB50F";

        auto-key-locate = "keyserver";
        keyserver = "hkps://hkps.pool.sks-keyservers.net";
        keyserver-options = "no-honor-keyserver-url include-revoked auto-key-retrieve";
      };
      scdaemonSettings = {
        card-timeout = "1";
        disable-ccid = true;
        pcsc-driver = "/System/Library/Frameworks/PCSC.framework/PCSC";
      };
    };

    ssh = {
      enable = true;

      controlMaster  = "auto";
      controlPath    = "${tmpdir}/ssh-%u-%r@%h:%p";
      controlPersist = "1800";

      forwardAgent = true;
      serverAliveInterval = 60;

      hashKnownHosts = true;
      userKnownHostsFile = "${config.xdg.configHome}/ssh/known_hosts";

      matchBlocks = {
        keychain = {
          host = "*";
          extraOptions = {
            UseKeychain    = "yes";
            AddKeysToAgent = "yes";
            IgnoreUnknown  = "UseKeychain";
          };
        };
      };
    };

    alacritty = {
      enable = true;
      settings = {
        font = {
          size = 12;
          normal = {
            family = "Source Code Pro";
          };
        };
        window = {
          decorations = "none";
        };
      };
    };
  };
}
