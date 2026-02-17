{
  description = "Elwood - Self-hosted personal AI assistant";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.ghc96;

        elwood = haskellPackages.callCabal2nix "elwood" ./. { };

      in {
        packages = {
          default = elwood;
          elwood = elwood;
        };

        devShells.default = haskellPackages.shellFor {
          packages = p: [ elwood ];

          buildInputs = with pkgs; [
            # Haskell tools
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.ormolu

            # System tools
            pkg-config
            zlib
          ];

          shellHook = ''
            echo "Elwood development shell"
            echo "GHC version: $(ghc --version)"
            echo ""
            echo "Commands:"
            echo "  cabal build    - Build the project"
            echo "  cabal run      - Run elwood"
            echo "  cabal test     - Run tests"
            echo ""
          '';
        };
      }
    ) // {
      # NixOS module
      nixosModules.default = import ./modules/assistant.nix;
      nixosModules.assistant = import ./modules/assistant.nix;

      # Overlay for adding elwood to pkgs
      overlays.default = final: prev: {
        elwood = self.packages.${prev.system}.default;
      };
    };
}
