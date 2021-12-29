# `xmonad-configuration`

Highly customized XMonad configuration.

Setup:

```bash
git clone github.com/SimSaladin/xmonad-configuration ~/.config/xmonad
./build
```

## Compiling via Nix

Uses IOHK's `haskell.nix` library:

```bash
nix-build -A xmonad
```

## `pre-commit` checks

Run `nix-shell` to run the `shellHook`, which:

1. installs `.git/hooks/pre-commit` and generate `.pre-commit-config.yaml`
2. provides a `pre-commit` executable in the shell to run the checks

Alternatively, you can build the attribute: `nix-build -A pre-commit-check`.


Uses [pre-commit-hooks.nix](https://github.com/cachix/pre-commit-hooks.nix).
