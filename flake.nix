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
      packages = forEachSupportedSystem ({ pkgs }: {
        apimock = pkgs.haskellPackages.callCabal2nix "apimock" ./. {};
        default = self.packages.${pkgs.system}.apimock;
      });

      devShells = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.mkShell {
          inputsFrom = [ self.packages.${pkgs.system}.default ];
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
