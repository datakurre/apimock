# Nix Development Environment

## Overview

To ensure a robust and reproducible development environment for Haskell, adherence to Nix Community best practices (specifically within `nixpkgs`/NixOS) is paramount.

## Best Practices

-   **Flake-based Development**: Utilizing `flake.nix` for defining development shells (`devShells`) and packages. This ensures consistent environments across different machines.
-   **`haskell.nix` Integration**: For complex Haskell projects, consider integrating `haskell.nix` to leverage its advanced capabilities for dependency management, multiple GHC versions, and robust builds.
-   **Pinning `nixpkgs`**: Always pin `nixpkgs` inputs to a specific commit or version to prevent unexpected breakage from `nixpkgs-unstable`.
-   **Explicit Dependencies**: Declare all Haskell dependencies explicitly in the `.cabal` file (or `package.yaml` for `hpack` projects) and ensure they are reflected in the Nix expressions.
-   **Build Reproducibility**: Prioritize `nix build` for reproducible artifacts over `cabal build` or `stack build` outside a controlled Nix environment.
-   **Debugging `nix build` failures**: When `nix build` fails, always inspect the build log (`nix log <derivation-hash>`) and trace (`--show-trace`) to understand the exact failure point. Issues often stem from incorrect `src` paths, missing `buildInputs`, or misconfigured phases.
-   **Avoid `impure` environments**: Strive to keep development and build environments as pure as possible. Use `--impure` only when strictly necessary for debugging or specific external dependencies.

## Historical Notes

During the initial setup, significant challenges were encountered in establishing a working Haskell development environment using both Nix flakes and Stack. Both approaches led to persistent build errors (e.g., `cabal2nix` not finding .cabal files, GHC installation failures due to `FP_PROG_LD_BUILD_ID` errors and permission denied issues). These indicate underlying environmental complexities or conflicts between Nix, Stack, and system-level dependencies. Further investigation into the host environment's Haskell toolchain setup is required for smooth development.
