self: super: {
  # TODO: Switch back to src build when SkyLight issue is fixed
  yabai = super.yabai.overrideAttrs (
    _: rec {
      version = "3.3.8";
      src = builtins.fetchTarball {
        url = "https://github.com/koekeishiya/yabai/releases/download/v${version}/yabai-v${version}.tar.gz";
        sha256 = "1qh1vf52j0b3lyrm005c8c98s39rk1lq61rrq0ml2yr4h77rq3xv";
      };

      installPhase = ''
        mkdir -p $out/bin
        mkdir -p $out/share/man/man1/
        cp ./bin/yabai $out/bin/yabai
        cp ./doc/yabai.1 $out/share/man/man1/yabai.1
      '';
    }
  );

  base16-shell = self.callPackage ../pkgs/base16-shell.nix {};

  jetbrains = super.jetbrains // {
    idea-ultimate = super.jetbrains.idea-ultimate.overrideAttrs (
      _: rec {
        version = "2021.3";
        src = super.fetchurl {
          url = "https://download.jetbrains.com/idea/ideaIU-${version}-no-jbr.tar.gz";
          sha256 = "0riwww75aizprb01c1sccprbr00ky5wgy5cxxjxqgm8v72rfnihb"; /* updated by script */
        };
      });
  };
}
