{ xmonad, xmonad-contrib, ... }:

let

  package =
    { lib, callCabal2nix, mkDerivation, makeWrapper, llvmPackages, alsa-plugins }:
    let
      src = lib.sourceByRegex ../.
        [
          "xmonad.hs"
          "xmonad-configuration.cabal"
          "lib.*"
          "README.md"
          "xmonad.svg"
          "LICENSE"
        ];

      alsa-plugins' = alsa-plugins.override { libjack2 = false; };

      mkDerivation_ = args: mkDerivation (args // {
        configureFlags = [ "-foptimize" "-fvia-llvm" ];
        enableLibraryProfiling = false;
        enableSharedExecutables = true;
        executableToolDepends = [ makeWrapper llvmPackages.llvm ];
        isLibrary = false;
        # Fix alsa-plugins. There's a patch in nixpkgs#alsa-lib that adds support for the ALSA_PLUGIN_DIR variable.
        # We need to set it at runtime. Also, note that the alsa-plugins-wrapper script uses the wrong
        # variable name (it's broken).
        #
        # https://github.com/NixOS/nixpkgs/issues/6860
        postFixup = ''
          wrapProgram $out/bin/xmonad* --set ALSA_PLUGIN_DIR ${alsa-plugins'}/lib/alsa-lib
        '';
      });

    in
    callCabal2nix "xmonad-configuration" src { mkDerivation = mkDerivation_; };
in

final: prev:
{
  haskell = prev.haskell // {
    packageOverrides = hfinal: hprev: {
      # XXX: xmonad/xmonad-contrib overlays set only 'haskellPackages' entry.
      # we need the rest to use non-default ghc versions.
      xmonad = hfinal.callCabal2nix "xmonad" xmonad.outPath { };
      xmonad-contrib = hfinal.callCabal2nix "xmonad-contrib" xmonad-contrib.outPath { };
      xmonad-configuration = final.callPackage package {
        inherit (hfinal) callCabal2nix;
        inherit (hprev) mkDerivation;
      };
    };
  };
}
