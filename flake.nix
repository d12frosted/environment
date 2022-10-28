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
    home-manager.url = "github:nix-community/home-manager";
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    # Follows
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, darwin, home-manager, emacs-overlay }:
    let
      overlays = [
        emacs-overlay.overlay
        (import ./nix/overlays)
      ];
    in {
      darwinConfigurations.d12frosted = darwin.lib.darwinSystem {
        system = "aarch64-darwin"; # "x86_64-darwin";
        modules = [
          ./nix/darwin.nix
          home-manager.darwinModules.home-manager
          {
            nixpkgs.overlays = overlays;
          }
        ];
      };

      homeConfigurations.borysb =
        let system = "x86_64-linux";
            pkgs = nixpkgs.legacyPackages.${system};
        in home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [
            ./nix/home.nix
            ./nix/linux/xsession.nix
            ./nix/linux/services.nix
            {
              nixpkgs.config.allowUnfreePredicate = (pkg: true);
              nixpkgs.overlays = overlays;
              home = {
                username = "borysb";
                homeDirectory = "/home/borysb";
              };
            }
          ];
        };

    };
}
