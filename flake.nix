{
  description = "XMonad configuration";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    xmonad = {
      url = "github:xmonad/xmonad";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    xmonad-contrib = {
      url = "github:SimSaladin/xmonad-contrib";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.xmonad.follows = "xmonad";
    };
  };

  outputs = { self, nixpkgs, pre-commit-hooks, flake-utils, xmonad, xmonad-contrib }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlay = import nix/overlay.nix { inherit xmonad xmonad-contrib; };
        overlays = [ overlay xmonad.overlay xmonad-contrib.overlay ];
        pkgs = import nixpkgs { inherit system overlays; };
        pname = "xmonad-configuration";
      in
      {
        packages = {
          "${pname}" = pkgs.haskellPackages.${pname};
          "${pname}-fast" =
            pkgs.haskell.lib.compose.linkWithGold (
              pkgs.haskell.lib.compose.dontHaddock (
                pkgs.haskellPackages.${pname}.overrideAttrs (_: { configureFlags = [ ]; })
              ));

          # TODO
          "${pname}-ghc921" =
            let
              hp = pkgs.haskell.packages."ghc921";
            in
            hp.${pname};
        };

        defaultPackage = self.packages.${system}.${pname};
        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hpack.enable = true;
              hlint.enable = true;
              stylish-haskell.enable = true;
              shellcheck.enable = true;
              nixpkgs-fmt.enable = true;
            };
          };
        };

        devShells = {
          dev = pkgs.haskellPackages.shellFor {
            packages = p: [ p."${pname}" ];
            buildInputs = with pkgs.haskellPackages; [
              cabal-install
              ghcid
            ];
            withHoogle = true;
            inherit (self.checks.${system}.pre-commit-check) shellHook;
          };

          pre-commit-check = nixpkgs.legacyPackages.${system}.mkShellNoCC {
            inherit (self.checks.${system}.pre-commit-check) shellHook;
          };
        };

        devShell = self.devShells.${system}.dev;

        inherit overlay overlays;
      }
    );
}
