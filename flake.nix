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
                Chart-diagrams = hs.lib.doJailbreak hsuper.Chart-diagrams;
                # gi-cairo = hs.lib.addPkgconfigDepends hsuper.gi-cairo [
                #   final.pcre2
                #   final.xorg.libXdmcp
                # ];
                # gi-cairo-render =
                #   hs.lib.addPkgconfigDepends hsuper.gi-cairo-render [
                #     final.pcre2
                #     final.xorg.libXdmcp
                #   ];
                # gi-gio = hs.lib.addPkgconfigDepends hsuper.gi-gio [
                #   final.libselinux
                #   final.libsepol
                #   final.pcre
                #   final.pcre2
                #   final.util-linuxMinimal # provides lib 'mount'
                # ];
                # gi-glib =
                #   hs.lib.addPkgconfigDepends hsuper.gi-glib [ final.pcre2 ];
                # gi-gobject =
                #   hs.lib.addPkgconfigDepend hsuper.gi-gobject final.pcre2;
                # gi-harfbuzz = hs.lib.addPkgconfigDepends hsuper.gi-harfbuzz [
                #   final.freetype
                #   final.pcre2
                # ];
                # gi-pango = hs.lib.addPkgconfigDepends hsuper.gi-pango [
                #   final.fribidi
                #   final.libdatrie
                #   final.libselinux
                #   final.libsepol
                #   final.libthai
                #   final.pcre
                #   final.pcre2
                #   final.util-linuxMinimal # provides lib 'mount'
                #   final.xorg.libXdmcp
                # ];
                # gi-pangocairo =
                #   hs.lib.addPkgconfigDepends hsuper.gi-pangocairo [
                #     final.fribidi
                #     final.libdatrie
                #     final.libselinux
                #     final.libsepol
                #     final.libthai
                #     final.pcre
                #     final.pcre2
                #     final.util-linuxMinimal # provides lib 'mount'
                #     final.xorg.libXdmcp
                #   ];
                # haskell-gi =
                #   hs.lib.addPkgconfigDepends hsuper.haskell-gi [ final.pcre2 ];
                # haskell-gi-base =
                #   hs.lib.addPkgconfigDepends hsuper.haskell-gi-base
                #   [ final.pcre2 ];
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
