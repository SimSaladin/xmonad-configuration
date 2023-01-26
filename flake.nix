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
      #url = "github:SimSaladin/xmonad-contrib";
      url = "git+file:///home/sim/.config/xmonad/xmonad-contrib";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.xmonad.follows = "xmonad";
    };
  };

  outputs = { self, nixpkgs, pre-commit-hooks, flake-utils, xmonad, xmonad-contrib }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlay = import nix/overlay.nix { inherit xmonad xmonad-contrib; };
        overlays = [
          xmonad.overlay
          xmonad-contrib.overlay
          overlay
        ];
        pkgs = import nixpkgs { inherit system overlays; };
        pname = "xmonad-configuration";
        hslib = pkgs.haskell.lib;
      in
      {
        packages = {
          "${pname}-ghc922" = pkgs.haskell.packages."ghc922".${pname};
          "${pname}-ghc902" = pkgs.haskell.packages."ghc902".${pname};
          # default
          "${pname}" = hslib.compose.linkWithGold ( self.packages.${system}."${pname}-ghc922" );
          "${pname}-fast" =
            hslib.compose.linkWithGold (
              hslib.compose.dontHaddock (
                self.packages.${system}.${pname}.overrideAttrs (_: { configureFlags = [ ]; })
              ));
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
          ghc922 = let hp = pkgs.haskell.packages.ghc922; in hp.shellFor {
            packages = p: [
              p.${pname}
            ];
            buildInputs = with hp; [
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

        devShell = self.devShells.${system}.ghc922;

        inherit overlay overlays;
      }
    );
}
