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
        packages.xmonad-configuration.components.exes.xmonad-x86_64-linux.libs = lib.optionals useLLVM [ llvm ];
        # xmobar requires some libraries that reside in the xorg attrset
        packages.xmobar.components.library.libs = lib.mkForce (with pkgs.xorg; [ libXrandr libXrender libXpm ]);
      })
    ];
    #materialized = ./.materialized;
  };
in
project // {
  xmonad-x86_64-linux = project.hsPkgs.xmonad-configuration.components.exes.xmonad-x86_64-linux;
}
