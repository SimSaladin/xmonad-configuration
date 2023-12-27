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

  outputs = inputs@{ self, nixpkgs, pre-commit-hooks, flake-utils, git-ignore-nix, xmonad, xmonad-contrib, xmobar }:
    let
      pname = "xmonad-configuration";

      compiler = "ghc963";

      hoverlay = final: prev: hself: hprev: {

        # need >=1.1.0 for GHC 9.6
        rawfilepath = final.haskell.lib.overrideCabal hprev.rawfilepath rec {
          version = "1.1.0";
          editedCabalFile = null;
          revision = null;
          sha256 = "sha256-DFXsXPVciYadZ5wBlF4F0GvM0h0J2qEdhg5OpIld9xc=";
          src = final.fetchurl {
            url = "mirror://hackage/rawfilepath-${version}.tar.gz";
            sha256 = "sha256-N9ORojJmWSi1m1P7/aDx6yJo1DAzJwTHXvdht+/FSiI=";
          };
        };

        ${pname} = hself.callCabal2nix "${pname}" (git-ignore-nix.lib.gitignoreSource ./.) {
          #alsa-plugins' = alsa-plugins.override { libjack2 = false; };
          mkDerivation = args: hprev.mkDerivation (args // {
            configureFlags = [
              "-foptimize"
              "-fvia-llvm"
            ];
            enableLibraryProfiling = false;
            enableSharedExecutables = true;
            executableToolDepends = [ final.makeWrapper final.llvmPackages_15.llvm ];

            #isLibrary = false;

            # Fix alsa-plugins. There's a patch in nixpkgs#alsa-lib that adds support for the ALSA_PLUGIN_DIR variable.
            # We need to set it at runtime. Also, note that the alsa-plugins-wrapper script uses the wrong
            # variable name (it's broken).
            #
            # https://github.com/NixOS/nixpkgs/issues/6860
            #postFixup = ''
            #  wrapProgram $out/bin/xmonad* --set ALSA_PLUGIN_DIR ${final.alsa-plugins}/lib/alsa-lib
            #'';
          });
        };
      };

      overlay = xmonad.lib.fromHOL hoverlay { };

      overlays = [
        (final: prev: {
          haskellPackages = prev.haskell.packages.${compiler};
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
          pkgs = import nixpkgs {
            inherit system overlays;
          };
          hpkg = pkgs.haskellPackages;
        in
        {
          packages = with pkgs.haskell.lib.compose; rec {
            default = linkWithGold (enableCabalFlag "via-llvm" (enableCabalFlag "optimize" (dontHaddock hpkg.${pname})));
            "${pname}" = default;
            "${pname}-fast" = linkWithGold (dontHaddock hpkg.${pname});
            # with haddocks
            "${pname}-full" = linkWithGold hpkg.${pname};
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
            default = hpkg.shellFor {
              packages = p: [ p.${pname} ];
              #nativeBuildInputs = [ hpkg.cabal-install hpkg.ghcid ];
              #withHoogle = true;
              inherit (self.checks.${system}.pre-commit-check) shellHook;
            };

            pre-commit-check = nixpkgs.legacyPackages.${system}.mkShellNoCC {
              inherit (self.checks.${system}.pre-commit-check) shellHook;
            };
          };
        }
      ) // {
      overlays.default = overlay;
      # inherit overlay overlays;
    };
}
