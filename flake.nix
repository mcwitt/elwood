{
  description = "Elwood - Self-hosted personal AI assistant";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
    git-hooks.url = "github:cachix/git-hooks.nix";
    weeder-nix.url = "github:NorfairKing/weeder-nix";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      git-hooks,
      weeder-nix,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        # Overlay to add elwood to haskellPackages
        haskellOverlay = final: prev: {
          haskell = prev.haskell // {
            packages = prev.haskell.packages // {
              ghc96 = prev.haskell.packages.ghc96.extend (
                hfinal: hprev: {
                  elwood = hfinal.callCabal2nix "elwood" ./. { };
                }
              );
            };
          };
        };

        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            haskellOverlay
            weeder-nix.overlays.${system}
          ];
        };

        haskellPackages = pkgs.haskell.packages.ghc96;

        elwood = haskellPackages.elwood;

      in
      {
        packages = {
          default = elwood;
          elwood = elwood;
        };

        devShells.default =
          let
            inherit (self.checks.${system}) pre-commit-check;
          in
          haskellPackages.shellFor {
            packages = p: [ p.elwood ];

            buildInputs =
              with pkgs;
              [
                # Haskell tools
                haskellPackages.cabal-install
                haskellPackages.haskell-language-server
                haskellPackages.hlint
                haskellPackages.ormolu
                haskellPackages.weeder

                # System tools
                d2
                pkg-config
                zlib
              ]
              ++ pre-commit-check.enabledPackages;

            shellHook = ''
              echo "Elwood development shell"
              echo "GHC version: $(ghc --version)"
              echo ""
              echo "Commands:"
              echo "  cabal build    - Build the project"
              echo "  cabal run      - Run elwood"
              echo "  cabal test     - Run tests"
              echo ""
            ''
            + pre-commit-check.shellHook;
          };

        formatter =
          let
            inherit (self.checks.${system}.pre-commit-check.config) package configFile;
            script = ''
              ${pkgs.lib.getExe package} run --all-files --config ${configFile}
            '';
          in
          pkgs.writeShellScriptBin "pre-commit-run" script;

        checks =
          let
            # NixOS module tests (only on Linux)
            nixosTests = if pkgs.stdenv.isLinux then import ./modules/tests { inherit pkgs self; } else { };
          in
          {
            pre-commit-check = git-hooks.lib.${system}.run {
              src = ./.;
              hooks = {
                cabal-fmt.enable = true;
                hlint.enable = true;
                ormolu.enable = true;
                nixfmt.enable = true;
                d2 = {
                  enable = true;
                  name = "d2-architecture-diagram";
                  entry = "${pkgs.lib.getExe' pkgs.d2 "d2"} --layout=elk docs/architecture.d2 docs/architecture.svg";
                  files = "(docs/architecture\\.d2|docs/architecture\\.svg)$";
                  pass_filenames = false;
                };
              };
            };

            weeder = pkgs.weeder-nix.makeWeederCheck {
              haskellPackages = haskellPackages;
              packages = [ "elwood" ];
              weederToml = ./weeder.toml;
            };
          }
          // nixosTests;
      }
    )
    // {
      # NixOS module
      nixosModules.default = import ./modules/assistant.nix;
      nixosModules.assistant = import ./modules/assistant.nix;

      # Overlay for adding elwood to pkgs
      overlays.default = final: prev: {
        elwood = self.packages.${prev.system}.default;
      };
    };
}
