{
  inputs = {
    #nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs.url = "nixpkgs";
  };

  outputs = { self, nixpkgs }:
  let
    project_name = "easy-mode";
    supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    eachSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f rec {
      inherit system;
      pkgs = nixpkgs.legacyPackages.${system};
      hpkgs = pkgs.haskell.packages.ghc98;
    });
  in
  rec {
    packages = eachSystem ({hpkgs, ...}: {
      default = hpkgs.callCabal2nix project_name ./. { };
    });

    devShells = eachSystem ({pkgs, hpkgs, system, ...}: {
      default = pkgs.haskell.lib.addBuildTools packages.${system}.default
        (with hpkgs; [ haskell-language-server cabal-install ]);
    });
  };
}
