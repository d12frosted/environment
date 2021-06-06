# References:
#
# https://github.com/cmacrae/config
# https://www.tweag.io/blog/2020-05-25-flakes/
# https://www.tweag.io/blog/2020-06-25-eval-cache/
# https://www.tweag.io/blog/2020-07-31-nixos-flakes/

{
  description = "d12frosted systems configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    darwin.url = "github:lnl7/nix-darwin";
    home.url = "github:nix-community/home-manager";
    emacs.url = "github:cmacrae/emacs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    spacebar.url = "github:cmacrae/spacebar";

    # Follows
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, darwin, home, emacs, emacs-overlay, spacebar }:
    let
      overlays = [
        emacs-overlay.overlay
        (import ./nix/overlays)
      ];
    in {
      darwinConfigurations.d12frosted = darwin.lib.darwinSystem {
        modules = [
          ./nix/darwin.nix
          home.darwinModules.home-manager
          {
            nixpkgs.overlays = [
              emacs.overlay
              spacebar.overlay
            ] ++ overlays;
          }
        ];
      };

      homeConfigurations = {
        borysb = home.lib.homeManagerConfiguration {
          configuration = { pkgs, lib, config, ... }: {
            imports = [ ./nix/home.nix ];
            nixpkgs.config.allowUnfree = true;
            nixpkgs.overlays = overlays;
          };
          system = "x86_64-linux";
          homeDirectory = "/home/d12frosted";
          username = "d12frosted";
        };
      };
    };
}
