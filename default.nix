let
 haskellNixSrc = builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz;
 haskellNix = import haskellNixSrc {};
 nixpkgsSrc = haskellNix.sources.nixpkgs-2111;
 pkgs = import nixpkgsSrc haskellNix.nixpkgsArgs;
 compiler-nix-name = "ghc8107";
 useLLVM = false; # true;
 llvm = pkgs.llvm_12;

  project = pkgs.haskell-nix.stackProject' {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "xmonad-configuration";
      src = ./.;
    };
    inherit compiler-nix-name;
    modules = [({pkgs, lib, ...}: {
      # Optimize and use LLVM
      packages.xmonad-configuration.flags.optimize = true;
      packages.xmonad-configuration.flags.via-llvm = useLLVM;
      packages.xmonad-configuration.components.exes.xmonad-x86_64-linux.libs = lib.optionals useLLVM [ llvm ];
      packages.xmonad-configuration.components.library.libs = lib.optionals useLLVM [ llvm ];
      # xmobar requires some libraries that reside in the xorg attrset
      packages.xmobar.components.library.libs = lib.mkForce (with pkgs.xorg; [ libXrandr libXrender libXpm ]);
    })];
    #materialized = ./.materialized;
  };
in project // {
  xmonad-x86_64-linux = project.hsPkgs.xmonad-configuration.components.exes.xmonad-x86_64-linux;
}
