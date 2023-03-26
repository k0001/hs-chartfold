{
  description = "Haskell 'Chart-stream' library";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs?rev=21eda9bc80bef824a037582b1e5a43ba74e92daa";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      flake.overlays.default = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = prev.lib.composeExtensions
            (prev.haskell.packageOverrides or (_: _: { })) (hself: hsuper: {
              chartfold = hself.callPackage ./chartfold { };
              chartfold-backend-Chart =
                hself.callPackage ./chartfold-backend-Chart { };
              Chart = prev.haskell.lib.doJailbreak hsuper.Chart;
            });
        };
      };
      systems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      perSystem = { config, system, pkgs, ... }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ inputs.self.overlays.default ];
        };
        packages = {
          default = pkgs.releaseTools.aggregate {
            name = "every output from this flake";
            constituents = [
              config.packages.chartfold__ghc943
              config.packages.chartfold__ghc943.doc
              config.devShells.ghc943

              config.packages.chartfold-backend-Chart__ghc943
              config.packages.chartfold-backend-Chart__ghc943.doc
              config.devShells.ghc943
            ];
          };
          chartfold__ghc943 = pkgs.haskell.packages.ghc943.chartfold;
          chartfold-backend-Chart__ghc943 =
            pkgs.haskell.packages.ghc943.chartfold-backend-Chart;
        };
        devShells = {
          default = config.devShells.ghc943;
          ghc943 = pkgs.haskell.packages.ghc943.shellFor {
            packages = p: [ p.chartfold p.chartfold-backend-Chart ];
            withHoogle = true;
            nativeBuildInputs = [ pkgs.cabal-install pkgs.cabal2nix ];
          };
        };
      };
    };
}
