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
                  checks = forEachSupportedSystem ({ pkgs }:
                    let
                      apimockPkg = pkgs.haskellPackages.apimock;
                    in
                    {
                      apimock-test = apimockPkg.tests.apimock-test;
                      apimock-test-coverage = apimockPkg.tests.apimock-test.overrideAttrs (old: {
                        doCoverage = true;
                      });
                    }
                  );
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
            zlib
            jq
          ];
        };
      });
    };
}
