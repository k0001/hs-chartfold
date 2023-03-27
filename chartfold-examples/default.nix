{ mkDerivation, base, base-prelude, Chart, Chart-diagrams, chartfold
, diagrams-canvas, lib, mtl-prelude, time }:
mkDerivation {
  pname = "chartfold-examples";
  version = "0.1";
  src = lib.sources.cleanSource ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends =
    [ base base-prelude Chart chartfold mtl-prelude time ];
  executableHaskellDepends = [
    base
    base-prelude
    Chart
    Chart-diagrams
    chartfold
    diagrams-canvas
    mtl-prelude
  ];
  homepage = "https://github.com/k0001/hs-chartfold";
  description = "Examples of using the chartfold library";
  license = lib.licenses.cc0;
  mainProgram = "chartfold-examples";
}
