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
    emacs.url = "github:cmacrae/emacs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    spacebar.url = "github:cmacrae/spacebar";

    # Follows
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, darwin, home-manager, emacs, emacs-overlay, spacebar }:
    let
      overlays = [
        emacs-overlay.overlay
        (import ./nix/overlays)
      ];
    in {
      darwinConfigurations.d12frosted = darwin.lib.darwinSystem {
        modules = [
          ./nix/darwin.nix
          home-manager.darwinModules.home-manager
          {
            nixpkgs.overlays = [
              emacs.overlay
              spacebar.overlay
            ] ++ overlays;
          }
        ];
      };

      nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./nix/nixos.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.d12frosted = import ./nix/home.nix;
            nixpkgs.overlays = overlays;
          }
        ];
      };
    };
}
