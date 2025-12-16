{
  description = "A very basic flake for a Haskell project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs { inherit system; };
      });
    in
    {
      devShells = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.mkShell {
          packages = with pkgs; [
            (haskellPackages.ghcWithPackages (ps: with ps; [ ]))
            cabal-install
            haskell-language-server
            fourmolu
            hlint
            ghcid
            haskellPackages.implicit-hie
            haskellPackages.cabal-fmt
            stack
          ];
        };
      });
    };
}
