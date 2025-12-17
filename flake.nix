{
  description = "API mock server based on OpenAPI specifications";

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
        apimock = (pkgs.haskellPackages.callCabal2nix "apimock" ./. {}).overrideAttrs (old: {
          doCheck = true;
        });
        default = self.packages.${pkgs.stdenv.hostPlatform.system}.apimock;
      });

      devShells = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.mkShell {
          inputsFrom = [ self.packages.${pkgs.stdenv.hostPlatform.system}.default ];
          packages = with pkgs; [
            cabal-install
            haskell-language-server
            fourmolu
            hlint
            ghcid
            haskellPackages.implicit-hie
            haskellPackages.cabal-fmt
            zlib
          ];
        };
      });
    };
}
