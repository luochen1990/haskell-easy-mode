{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hpkgs = pkgs.haskell.packages.ghc928;
      in rec {
        defaultPackage = hpkgs.callCabal2nix "easy-mode" ./. { };
        devShell = pkgs.haskell.lib.addBuildTools defaultPackage
          (with hpkgs; [ haskell-language-server cabal-install hpack hoogle ]);
      });
}
