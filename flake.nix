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
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.gitignore.follows = "git-ignore-nix";
    };

    xmonad = {
      url = "github:xmonad/xmonad";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.unstable.follows = "nixpkgs";
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
      #url = "git+https://codeberg.org/xmobar/xmobar";
      url = "github:SimSaladin/xmobar";
      #url = "path:/home/sim/xmobar";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, pre-commit-hooks, flake-utils, git-ignore-nix, xmonad, xmonad-contrib, xmobar }:
    let
      lib = nixpkgs.lib;

      pname = "xmonad-configuration";

      allGhcVersions = lib.subtractLists [ "ghc810" "ghc90" "ghc92" "ghcjs810" "ghcjs" "integer-simple" "native-bignum" ] (lib.attrNames nixpkgs.legacyPackages.x86_64-linux.haskell.packages);

      hoverlay = final: _prev: hself: hprev:
        let
          #ghcVersion = lib.replaceStrings [ "-" "." ] [ "" "" ] hself.ghc.haskellCompilerName;
          llvmPackages =
            #if lib.versionAtLeast hself.ghc.version "9.10" then final.llvmPackages_19 # hself.ghc.llvmPackages # XXX: ?
            if lib.versionAtLeast hself.ghc.version "9.10" then hself.ghc.llvmPackages
            else final.llvmPackages_15;
        in
        {
          ${pname} = hself.callCabal2nixWithOptions pname (git-ignore-nix.lib.gitignoreSource ./.) "--hpack" {
            mkDerivation = args: hprev.mkDerivation (args // {
              enableLibraryProfiling = false;
              enableSharedExecutables = true;
              buildTools = args.buildTools or [ ] ++ [
                llvmPackages.llvm
                llvmPackages.clang # Appears to be needed since GHC-9.10.1
              ];
            });
          };

          xmobar = (final.haskell.lib.overrideSrc hprev.xmobar {
            src = xmobar;
            version = "dev-${xmobar.shortRev or "dirty"}";
          }).overrideAttrs {
            #patches = [ ./xmobar-hidpi-auto-height.patch ];
          };

          # Patch null pointer exception causing segfault when font cannot be
          # loaded
          X11-xft = hprev.X11-xft.overrideAttrs (oa: {
            # XXX weird unique needed
            patches = lib.unique (oa.patches or [ ] ++ [
              (final.fetchpatch {
                url = "https://github.com/xmonad/X11-xft/pull/21.diff";
                hash = "sha256-Y7iSjU695jbgAvoqBLtIsWIFj4TFKMQ98R2visn0Z+0=";
                postFetch = "sed -i -e '/X11-xft.cabal/,/author:/ d' $out";
              })
            ]);
          });

          #pango = final.haskell.lib.overrideSrc hprev.pango rec {
          #  src = final.fetchurl {
          #     url = "mirror://hackage/pango-${version}.tar.gz";
          #  };
          #  version = "0.13.12.0";
          #};

          # https://github.com/jgoerzen/configfile/pull/12
          ConfigFile = hself.callCabal2nix "ConfigFile"
            (final.fetchFromGitHub {
              owner = "rvl";
              repo = "configfile";
              rev = "83ee30b43f74d2b6781269072cf5ed0f0e00012f";
              hash = "sha256-RfL6a5JdWhf2nrBaWN/7GSfHgQXJlpyBArNyLUvdq4s=";
            })
            { };

          # > ghc9101 requires at least 1.4.0
          dbus =
            if lib.versionAtLeast hself.ghc.version "9.10.1" && lib.versionOlder hprev.dbus.version "1.4.0" then
              hself.callHackageDirect
                {
                  pkg = "dbus";
                  ver = "1.4.0";
                  sha256 = "sha256-QnI+Sc4ZVpejjb+f9it24BS4mrhqOfO1MSkRm8z9NL8=";
                }
                { } else hprev.dbus;

          # dbus>=1.4.0 needs at least >=3.2
          network =
            if lib.versionAtLeast hself.ghc.version "9.10.1" && lib.versionOlder hprev.network.version "3.2" then
              hself.callHackageDirect
                {
                  pkg = "network";
                  ver = "3.2.7.0";
                  sha256 = "sha256-QJCy1P8R/ghy1Y9odCNgEaDsgaTu10qxSksIUl+q2SE=";
                }
                { } else hprev.network;

          ##
          # GHC >= 9.12.1
          #indexed-traversable = final.haskell.lib.doJailbreak hprev.indexed-traversable;
          #indexed-traversable-instances = final.haskell.lib.doJailbreak hprev.indexed-traversable-instances;
          #semialign = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.doJailbreak hprev.semialign else hprev.semialign;
          #ChasingBottoms = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.doJailbreak hprev.ChasingBottoms else hprev.ChasingBottoms;
          #alex = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.dontCheck hprev.alex else hprev.alex;
          #xml-conduit = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.dontCheck hprev.xml-conduit else hprev.xml-conduit;
          #interpolate = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.dontCheck hprev.interpolate else hprev.interpolate;
          #crypton = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.dontCheck hprev.crypton else hprev.crypton;
          #doctest = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.dontCheck (final.haskell.lib.doJailbreak hprev.doctest) else hprev.doctest;
          #generic-deriving = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.doJailbreak hprev.generic-deriving else hprev.generic-deriving;
          #happy = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.dontCheck hprev.happy else hprev.happy;
          #hashable = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.doJailbreak hprev.hashable else hprev.hashable;
          #integer-logarithms = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.doJailbreak hprev.integer-logarithms else hprev.integer-logarithms;
          #integer-conversion = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.doJailbreak hprev.integer-conversion else hprev.integer-conversion;
          #nothunks = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.doJailbreak hprev.nothunks else hprev.nothunks;
          #scientific = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.doJailbreak hprev.scientific else hprev.scientific;
          #setlocale = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.doJailbreak hprev.setlocale else hprev.setlocale;
          #th-abstraction =
          #  if lib.versionAtLeast hself.ghc.version "9.12.1" then
          #    hself.callHackageDirect
          #      {
          #        pkg = "th-abstraction";
          #        ver = "0.7.1.0";
          #        sha256 = "sha256-XZ8f1KnMszsFitzN1qsWEOLN09yqhhaR9tn/u1I/mSc=";
          #      }
          #      { } else hprev.th-abstraction;
          #th-compat = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.doJailbreak hprev.th-compat else hprev.th-compat;
          #th-expand-syns = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.doJailbreak hprev.th-expand-syns else hprev.th-expand-syns;
          #th-lift = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.doJailbreak hprev.th-lift else hprev.th-lift;
          #th-orphans = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.doJailbreak hprev.th-orphans else hprev.th-orphans;
          #these = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.doJailbreak hprev.these else hprev.these;
          #time-compat = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.doJailbreak hprev.time-compat else hprev.time-compat;
          #timezone-olson = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.doJailbreak hprev.timezone-olson else hprev.timezone-olson;
          #timezone-series = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.doJailbreak hprev.timezone-series else hprev.timezone-series;
          #uuid-types = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.doJailbreak hprev.uuid-types else hprev.uuid-types;
          #zlib = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.doJailbreak hprev.zlib else hprev.zlib;
          #hpack = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.doJailbreak hprev.hpack else hprev.hpack;
          #invariant = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.doJailbreak hprev.invariant else hprev.invariant;
          #quickcheck-instances = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.doJailbreak hprev.quickcheck-instances else hprev.quickcheck-instances;
          #haskell-src-meta = if lib.versionAtLeast hself.ghc.version "9.12.1" then final.haskell.lib.doJailbreak hprev.haskell-src-meta else hprev.haskell-src-meta;
          #lens =
          #  if lib.versionAtLeast hself.ghc.version "9.12.1" then
          #    hself.callHackageDirect
          #      {
          #        pkg = "lens";
          #        ver = "5.3.4";
          #        sha256 = "sha256-g6pzTgFe1+bAAwRUjyHGbVZ8jGc1/01HbAGZEuqktZ8=";
          #      }
          #      { } else hprev.lens;
          #aeson =
          #  if lib.versionAtLeast hself.ghc.version "9.12.1" then
          #    final.haskell.lib.doJailbreak
          #      (hself.callHackageDirect
          #        {
          #          pkg = "aeson";
          #          ver = "2.2.3.0";
          #          sha256 = "sha256-V4cld9jCPuzpMMAOJXeyWyVPwE05zcmZLpMtSc0HKqk=";
          #        }
          #        { }) else hprev.aeson;
          #attoparsec-aeson =
          #  if lib.versionAtLeast hself.ghc.version "9.12.1" then
          #    hself.callHackageDirect
          #      {
          #        pkg = "attoparsec-aeson";
          #        ver = "2.2.2.0";
          #        sha256 = "sha256-B8STXlw/3bkY2eVG1npkzRfRqgUMECG4vRC2GtjsKjA=";
          #      }
          #      { } else hprev.attoparsec-aeson;
          #extra =
          #  if lib.versionAtLeast hself.ghc.version "9.12.1" then
          #    hself.callHackageDirect
          #      {
          #        pkg = "extra";
          #        ver = "1.8";
          #        sha256 = "sha256-vba47lUzbaN3Ki6D/1j67Kc/GoZ5bPCI2O5MYJlN0zI=";
          #      }
          #      { } else hprev.extra;
          #gtk2hs-buildtools =
          #  if lib.versionAtLeast hself.ghc.version "9.12.1" then
          #    hself.callHackageDirect
          #      {
          #        pkg = "gtk2hs-buildtools";
          #        ver = "0.13.12.0";
          #        sha256 = "sha256-0YRqeKyNmmPmlHg7gBpiu5haf/Eksr61yszGToe0x2I=";
          #      }
          #      { } else hprev.gtk2hs-buildtools;
          #pango = if lib.versionAtLeast hself.ghc.version "9.12.1" then hself.callHackageDirect {
          #  pkg = "pango";
          #  ver = "0.13.12.0";
          #  #sha256 = lib.fakeHash;
          #  sha256 = "sha256-bDB0An98YyiOjuIAF2tp4FxCstYJD4V7yI8sf6I5RsI=";
          #} { } else hprev.pango;
        };

      # For all GHC versions
      fromHOL' = hov: lib.composeManyExtensions (lib.map (compiler: xmonad.lib.fromHOL hov { inherit compiler; }) (allGhcVersions));

      overlays = [
        # build cabal2nix with a different package set as suggested by https://github.com/NixOS/nixpkgs/issues/83098#issuecomment-602132784
        (_self: super: {
          cabal2nix-unwrapped = super.haskell.lib.justStaticExecutables
            (super.haskellPackages.generateOptparseApplicativeCompletions [ "cabal2nix" ] super.haskell.packages."ghc984".cabal2nix);
        })
        (fromHOL' xmonad.hoverlay)
        (fromHOL' xmonad-contrib.hoverlay)
        (fromHOL' hoverlay)
      ];

      perSystem = system:
        let
          pkgs = import nixpkgs { inherit system overlays; };
          hp = pkgs.haskellPackages;
          eachGHC = f: lib.foldr (a: b: a // b) { } (lib.map
            (ghcname:
              lib.mapAttrs' (k: v: lib.nameValuePair "${ghcname}:${k}" v) (f pkgs.haskell.packages.${ghcname})
            )
            allGhcVersions);
          mkDevShell = hp: hp.shellFor {
            packages = hpkgs: [
              hpkgs.${pname}
              hpkgs.gtk3 # for pango(gtk2hs) testing
              hpkgs.pango # for xmobar
            ];
            nativeBuildInputs = with pkgs; [ cabal-install hpack ];
            inherit (self.checks.${system}.pre-commit-check) shellHook;
          };
        in
        {
          packages = with pkgs.haskell.lib.compose; {
            default = self.packages.${system}.${pname};
            ${pname} = lib.pipe hp.${pname} [
              dontHaddock
              (enableCabalFlag "via-llvm")
              (enableCabalFlag "optimize")
              linkWithGold
            ];
            "${pname}-fast" = lib.pipe hp.${pname} [
              dontHaddock
              disableOptimization
              linkWithGold
            ];
            xmobar = hp.xmobar;
          } // lib.fold (a: b: a // b) { } (lib.map
            (comp: {
              "${comp}/${pname}" = lib.pipe pkgs.haskell.packages.${comp}.${pname} [
                dontHaddock
                (enableCabalFlag "via-llvm")
                (enableCabalFlag "optimize")
                linkWithGold
              ];
            })
            allGhcVersions);

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
            default = mkDevShell hp;
            minimal = hp.shellFor {
              packages = p: [ p.xmonad p.xmonad-contrib ];
              nativeBuildInputs = [ pkgs.hpack ];
            };
            pre-commit-check = pkgs.mkShellNoCC {
              inherit (self.checks.${system}.pre-commit-check) shellHook;
            };
          } // eachGHC (hp: {
            default = mkDevShell hp;
          });

          inherit pkgs;
        };
    in
    flake-utils.lib.eachSystem [ "x86_64-linux" ] perSystem // {
      overlays = {
        default = lib.composeManyExtensions overlays;
      };
      inherit hoverlay;
    };
}
