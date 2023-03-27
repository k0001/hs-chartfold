{
  description = "Haskell 'Chart-stream' library";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs?rev=21eda9bc80bef824a037582b1e5a43ba74e92daa";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      flake.overlays.default = final: prev:
        let
          lib = prev.lib;
          hs = prev.haskell;
          hsAddPropagatePkgconfigDepends = d: xs:
            (hs.lib.addPkgconfigDepends d xs).overrideAttrs (old: {
              propagatedBuildInputs = xs ++ (old.propagatedBuildInputs or [ ]);
            });
        in {
          haskell = prev.haskell // {
            packageOverrides =
              lib.composeExtensions (hs.packageOverrides or (_: _: { }))
              (hself: hsuper: {
                # Internal
                chartfold = hself.callPackage ./chartfold { };
                chartfold-examples = hself.callPackage ./chartfold-examples { };
                chartfold-backend-Chart =
                  hself.callPackage ./chartfold-backend-Chart { };

                # External
                Chart = hs.lib.doJailbreak hsuper.Chart;
                Chart-cairo = hs.lib.doJailbreak hsuper.Chart-cairo;
                gtk = hsAddPropagatePkgconfigDepends
                  (hs.lib.doJailbreak hsuper.gtk) [
                    final.fribidi
                    final.libdatrie
                    final.libdeflate
                    final.libthai
                    final.xorg.libXdmcp
                  ];
                gio = hsAddPropagatePkgconfigDepends hsuper.gio [
                  final.libselinux
                  final.libsepol
                  final.pcre
                  final.pcre2
                  final.util-linuxMinimal # provides libmount
                ];
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

              config.packages.chartfold-backend-Chart__ghc943
              config.packages.chartfold-backend-Chart__ghc943.doc

              config.packages.chartfold-examples__ghc943
              config.packages.chartfold-examples__ghc943.doc

              config.devShells.ghc943
            ];
          };
          chartfold__ghc943 = pkgs.haskell.packages.ghc943.chartfold;
          chartfold-examples__ghc943 =
            pkgs.haskell.packages.ghc943.chartfold-examples;
          chartfold-backend-Chart__ghc943 =
            pkgs.haskell.packages.ghc943.chartfold-backend-Chart;
        };
        devShells = {
          default = config.devShells.ghc943;
          ghc943 = pkgs.haskell.packages.ghc943.shellFor {
            packages = p: [
              p.chartfold
              p.chartfold-backend-Chart
              p.chartfold-examples
            ];
            withHoogle = true;
            nativeBuildInputs = [ pkgs.cabal-install pkgs.cabal2nix ];
          };
        };
      };
    };
}
