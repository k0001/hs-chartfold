{ mkDerivation, base, base-prelude, Chart, Chart-cairo, chartfold
, chartfold-backend-Chart, containers, gtk, lib, mtl-prelude, stm
, time
}:
mkDerivation {
  pname = "chartfold-examples";
  version = "0.1";
  src = lib.sources.cleanSource ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base base-prelude Chart Chart-cairo chartfold
    chartfold-backend-Chart containers gtk mtl-prelude stm time
  ];
  homepage = "https://github.com/k0001/hs-chartfold";
  description = "Examples of using the chartfold library";
  license = lib.licenses.cc0;
  mainProgram = "chartfold-examples-clock";
}
