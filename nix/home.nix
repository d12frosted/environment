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
      ASPELL_CONF       = "dict-dir ${home}/.nix-profile/lib/aspell";
      BASE16_HOME       = "${pkgs.base16-shell}";
      EDITOR            = "${emacsclient}";
      EMACS_SERVER_FILE = "${emacs-server}";
      GNUPGHOME         = "${config.xdg.configHome}/gnupg";
      NIX_CONF          = "${config.xdg.configHome}/nix";
      PROJECTS_HOME     = "${home}/Developer";
      SSH_AUTH_SOCK     = "${config.xdg.configHome}/gnupg/S.gpg-agent.ssh";
      XDG_CACHE_HOME    = config.xdg.cacheHome;
      XDG_CONFIG_HOME   = config.xdg.configHome;
      XDG_DATA_HOME     = config.xdg.dataHome;
      FONTCONFIG_FILE   = "${config.xdg.configHome}/fontconfig/fonts.conf";
      FONTCONFIG_PATH   = "${config.xdg.configHome}/fontconfig";
      BUDGET_DIR        = "${home}/Dropbox/budget";
    };
    sessionPath = [];
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
       pinentry-program ${pkgs.pinentry-qt}/bin/pinentry-qt
    '';
  };

  fonts.fontconfig.enable = true;

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
      includes = [
        {
          path = "${config.xdg.configHome}/git/local.config";
        }
      ];
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

    fish = {
      enable = true;
      plugins = [
        {
          name = "base16-fish";
          src = pkgs.fetchFromGitHub {
            owner = "tomyun";
            repo = "base16-fish";
            rev = "2f6dd973a9075dabccd26f1cded09508180bf5fe";
            sha256 = "PebymhVYbL8trDVVXxCvZgc0S5VxI7I1Hv4RMSquTpA=";
          };
        }
        {
          name = "hydro";
          src = pkgs.fetchFromGitHub {
            owner = "jorgebucaran";
            repo = "hydro";
            rev = "a5877e9ef76b3e915c06143630bffc5ddeaba2a1";
              sha256 = "nJ8nQqaTWlISWXx5a0WeUA4+GL7Fe25658UIqKa389E=";
          };
        }
        {
          name = "done";
          src = pkgs.fetchFromGitHub {
            owner = "franciscolourenco";
            repo = "done";
            rev = "37117c3d8ed6b820f6dc647418a274ebd1281832";
            sha256 = "cScH1NzsuQnDZq0XGiay6o073WSRIAsshkySRa/gJc0=";
          };
        }
      ];
      shellInit = ''
# prompt configurations
set -g hydro_symbol_prompt "λ"
if test "$TERM" = linux
  set -g hydro_symbol_prompt ">"
end

# done configurations
set -g __done_notification_command 'notify send -t "$title" -m "$message"'
set -g __done_enabled 1
set -g __done_allow_nongraphical 1
set -g __done_min_cmd_duration 8000

# see https://github.com/LnL7/nix-darwin/issues/122
set -gp PATH /nix/var/nix/profiles/default/bin
set -gp PATH /run/current-system/sw/bin
set -gp PATH $HOME/.nix-profile/bin
set -gp PATH $HOME/.local/bin
set -gp PATH ${config.xdg.configHome}/bin

set -gp NIX_PATH nixpkgs=$HOME/.nix-defexpr/channels_root/nixpkgs
'';
      interactiveShellInit = ''
set fish_greeting "
                       &    &     &
                        &&&&  &  && &
                &   &     && &&&&&&&
                 &&& &       &&&& &&&
              &&&&& &&& &   /~&&& &&   &&
             && &&&&_&_&_ & \&&&&&&&& && &&
                  &   &&\&&&&__&&&&&&&_/&&&  & &
                         \|\\\__/_/   &&& &
                         \_   _//     & &
                               \\
                                \\
                                //~
                                \\
                                /~
                                 /~
             ;,'             (---./~~\.---)
     _o_    ;:;'   ,          (          )
 ,-.'---`.__ ;      ~,         (________)
((j`~=~=~',-'     ,____.
 `-\     /        |    j
    `-=-'          `--'
"

if test "$TERM" = "linux"
  clear
end
base16-tomorrow
      '';
      loginShellInit = ''
set -gx GPG_TTY (tty)
if ! pgrep -x "gpg-agent" > /dev/null
  ${pkgs.gnupg}/bin/gpgconf --launch gpg-agent
end
      '';
    };

    rofi = {
      enable = pkgs.stdenv.isLinux;
      font = "Source Code Pro 10";
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
