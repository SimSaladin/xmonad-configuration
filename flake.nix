{
  description = "XMonad configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";

    flake-utils.url = "github:numtide/flake-utils";

    git-ignore-nix = {
      url = "github:hercules-ci/gitignore.nix/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.gitignore.follows = "git-ignore-nix";
    };

    xmonad = {
      url = "github:xmonad/xmonad";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
    };

    xmonad-contrib = {
      url = "github:SimSaladin/xmonad-contrib?ref=develop-2";
      #url = "git+file:///home/sim/.config/xmonad/xmonad-contrib";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.xmonad.follows = "xmonad";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
    };

    xmobar = {
      url = "git+https://codeberg.org/xmobar/xmobar";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
    };
  };

  outputs = { self, nixpkgs, pre-commit-hooks, flake-utils, git-ignore-nix, xmonad, xmonad-contrib, xmobar }:
    let
      pname = "xmonad-configuration";
      ghcVersion = "ghc982";
      llvmVersion = 15; # Max LLVM version depends on GHC version.
      hoverlay = final: _prev: hself: hprev:
        {
          ${pname} = hself.callCabal2nixWithOptions pname (git-ignore-nix.lib.gitignoreSource ./.) "--hpack" {
            mkDerivation = args: hprev.mkDerivation (args // {
              enableLibraryProfiling = false;
              enableSharedExecutables = true;
              buildTools = args.buildTools or [ ] ++ [ final."llvmPackages_${toString llvmVersion}".llvm ];
            });
          };
          # https://github.com/jgoerzen/configfile/pull/12
          ConfigFile = hself.callCabal2nix "ConfigFile"
            (final.fetchFromGitHub {
              owner = "rvl";
              repo = "configfile";
              rev = "83ee30b43f74d2b6781269072cf5ed0f0e00012f";
              hash = "sha256-RfL6a5JdWhf2nrBaWN/7GSfHgQXJlpyBArNyLUvdq4s=";
            })
            { };
        };
      overlay = xmonad.lib.fromHOL hoverlay { };
      overlays = [
        # Need to use/set the haskellPackages attribute for the xmobar overlay...
        (_final: prev: {
          haskellPackages = prev.haskell.packages.${ghcVersion};
        })
        xmonad.overlay
        xmonad-contrib.overlay
        xmobar.overlay
        overlay
      ];
    in
    flake-utils.lib.eachSystem [ "x86_64-linux" ]
      (system:
        let
          lib = nixpkgs.lib;
          pkgs = import nixpkgs { inherit system overlays; };
          hpkg = pkgs.haskellPackages;
        in
        {
          packages = with pkgs.haskell.lib.compose; {
            default = self.packages.${pname};
            ${pname} = lib.pipe hpkg.${pname} [
              dontHaddock
              (enableCabalFlag "via-llvm")
              (enableCabalFlag "optimize")
              linkWithGold
            ];
            "${pname}-fast" = lib.pipe hpkg.${pname} [
              dontHaddock
              disableOptimization
              linkWithGold
            ];
          };

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
            minimal = hpkg.shellFor {
              packages = p: [ p.xmonad ];
              nativeBuildInputs = [ hpkg.hpack ];
            };
            default = hpkg.shellFor {
              packages = p: [ p.${pname} ];
              nativeBuildInputs = with hpkg; [ cabal-install hpack ];
              #withHoogle = true;
              inherit (self.checks.${system}.pre-commit-check) shellHook;
            };

            pre-commit-check = pkgs.mkShellNoCC {
              inherit (self.checks.${system}.pre-commit-check) shellHook;
            };
          };
        }
      ) // {
      overlays.default = overlay;
    };
}
