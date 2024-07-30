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
      ghc-h = hpkgs.ghcWithPackages (p: [ self.packages.${system}.default p.pretty-simple ]);
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

    apps = eachSystem ({pkgs, ghc, ghc-h, system, ...}: rec {
      default = ghci-h;
      ghci = {
        type = "app";
        program = "${pkgs.writeShellScript "${project-name}-ghci" ''
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
      ghci-h = {
        type = "app";
        program = "${pkgs.writeShellScript "${project-name}-ghci" ''
          exec ${ghc-h}/bin/ghci -ghci-script ${
            pkgs.writeText ".ghci" ''
              :set -XOverloadedStrings
              :set -XOverloadedRecordDot
              :set -XDuplicateRecordFields
              :set -XNoFieldSelectors
              :set -XNoImplicitPrelude
              :set -interactive-print=Text.Pretty.Simple.pPrint
              :set -fdiagnostics-color=always
              :set prompt "\ESC[35m\STX λ: \ESC[m\STX"
              :module + EasyMode
            ''
          }
        ''}";
      };
      main = {
        type = "app";
        program = "${packages.${system}.default}/bin/${project-name}";
      };
    });
  };
}
