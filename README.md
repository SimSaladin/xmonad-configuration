# `xmonad-configuration`

Highly customized XMonad configuration.

Setup:

```bash
git clone github.com/SimSaladin/xmonad-configuration ~/.config/xmonad
./build
```

## Build

```bash
nix build
```

## `pre-commit` checks

Uses [pre-commit-hooks.nix](https://github.com/cachix/pre-commit-hooks.nix).

Enter dev shell with `nix shell`. What it does:

1. installs `.git/hooks/pre-commit` and generate `.pre-commit-config.yaml`
2. provides a `pre-commit` executable in the shell to run the checks

Alternatively, you can build the attribute:

```bash
nix build .#checks.x86_64-linux.pre-commit-check
```

```bash
# Run flake checks
nix flake check

# Check all files
pre-commit run -a
```
