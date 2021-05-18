{
  description = "d12frosted systems configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    # nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-20.09-darwin";
    darwin.url = "github:lnl7/nix-darwin";
    home.url = "github:nix-community/home-manager";
    emacs.url = "github:cmacrae/emacs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    spacebar.url = "github:cmacrae/spacebar";

    # Follows
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, darwin, home, emacs, emacs-overlay, spacebar }: {
    darwinConfigurations.d12frosted = darwin.lib.darwinSystem {
      modules = [
        ./modules/darwin.nix
        home.darwinModules.home-manager
        {
          nixpkgs.overlays = [
            emacs.overlay
            emacs-overlay.overlay
            spacebar.overlay
          ];
        }
      ];
    };
  };
}
