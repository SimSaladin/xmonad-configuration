{ haskellNixSrc ? builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz
, haskellNix ? import haskellNixSrc {}
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2111
, nixpkgsArgs ? haskellNix.nixpkgsArgs
, compiler-nix-name ? "ghc8107"
, optimize ? true
, useLLVM ? true
, llvmVersion ? "llvm_12"
}:
let
  pkgs = import nixpkgsSrc nixpkgsArgs;
  llvm = pkgs.${llvmVersion};
  alsa-plugins = pkgs.alsa-plugins.override { libjack2 = false; };

  project = pkgs.haskell-nix.stackProject' {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "xmonad-configuration";
      src = ./.;
    };
    inherit compiler-nix-name;
    modules = [
      ({ pkgs, lib, ... }: {
        # Optimize and use LLVM
        packages.xmonad-configuration.flags.optimize = optimize;
        packages.xmonad-configuration.flags.via-llvm = useLLVM;
        packages.xmonad-configuration.components.exes.xmonad-x86_64-linux = {
          libs = lib.optionals useLLVM [ llvm ];
          # Fix alsa-plugins. There's a patch in nixpkgs#alsa-lib that adds support for the ALSA_PLUGIN_DIR variable.
          # We need to set it at runtime. Also, note that the alsa-plugins-wrapper script uses the wrong
          # variable name (it's broken).
          #
          # https://github.com/NixOS/nixpkgs/issues/6860
          build-tools = [ pkgs.makeWrapper ];
          postInstall = "wrapProgram $out/bin/xmonad-x86_64-linux --set ALSA_PLUGIN_DIR ${alsa-plugins}/lib/alsa-lib";
        };
        # xmobar requires some libraries that reside in the xorg attrset
        packages.xmobar.components.library.libs = lib.mkForce (with pkgs.xorg; [ libXrandr libXrender libXpm ]);
      })
    ];
  };
in
project // {
  xmonad-x86_64-linux = project.hsPkgs.xmonad-configuration.components.exes.xmonad-x86_64-linux;
}
