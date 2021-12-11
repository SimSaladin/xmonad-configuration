{ haskellNixSrc ? builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz
, haskellNix ? import haskellNixSrc {}
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2111
}:

let
  pkgs = import nixpkgsSrc haskellNix.nixpkgsArgs;

  myOverrides = {
    # xmobar requires some libraries that reside in the xorg attrset
    packages.xmobar.components.library.libs = pkgs.lib.mkForce (with pkgs.xorg; [ libXrandr libXrender libXpm ]);
    # Enable optimizations
    packages.xmonad-configuration.flags.optimize = true;
    # use LLVM
    #packages.xmonad-configuration.flags.via-llvm = true;
    #packages.xmonad-configuration.components.library.libs = [ pkgs.llvm_13 ];
    packages.xmonad-configuration.components.exes.xmonad-x86_64-linux.libs = [ pkgs.llvm_12 ];
    packages.xmonad-configuration.components.exes.xmonad-x86_64-linux.ghcOptions = [ "-fllvm" ];
    doHoogle = false;
    doHaddock = false;
    reinstallableLibGhc = true;
  };

  project = pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "xmonad-configuration";
      src = ./.;
    };
    compiler-nix-name = "ghc8107";
    modules = [ myOverrides ];
  };
in {
  inherit pkgs project;
  xmonad = project.xmonad-configuration.components.exes.xmonad-x86_64-linux;
}
