{
  inputs = {
    #nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs.url = "nixpkgs";
  };

  outputs = { self, nixpkgs }:
  let
    project-name = "easy-mode";
    ghc-version = "ghc98";
    supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    eachSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f rec {
      inherit system;
      pkgs = nixpkgs.legacyPackages.${system};
      hpkgs = pkgs.haskell.packages.${ghc-version};
      ghc = hpkgs.ghcWithPackages (p: [ self.packages.${system}.default ]);
    });
  in
  rec {
    packages = eachSystem ({hpkgs, ...}: {
      default = hpkgs.callCabal2nix project-name ./. { };
    });

    devShells = eachSystem ({pkgs, hpkgs, system, ...}: {
      default = pkgs.haskell.lib.addBuildTools packages.${system}.default
        (with hpkgs; [ haskell-language-server cabal-install ]);
    });

    apps = eachSystem ({pkgs, ghc, system, ...}: {
      default = {
        type = "app";
        #program = "${packages.${system}.default}/bin/${project_name}";
        program = "${pkgs.writeShellScript "${project-name}-ghci" ''
          #export GHC_PACKAGE_PATH=${ "${packages.${system}.default}/" }
          exec ${ghc}/bin/ghci -ghci-script ${
            pkgs.writeText ".ghci" ''
              :set -XOverloadedStrings
              :set -XOverloadedRecordDot
              :set -XDuplicateRecordFields
              :set -XNoFieldSelectors
              :set -XNoImplicitPrelude
              :module + EasyMode
            ''
          }
        ''}";
      };
    });
  };
}
