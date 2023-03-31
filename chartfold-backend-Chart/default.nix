{ mkDerivation, base, base-prelude, Chart, chartfold, constraints
, containers, data-default-class, lens, lib, text, vector-space
, parallel
}:
mkDerivation {
  pname = "chartfold-backend-Chart";
  version = "0.1";
  src = lib.sources.cleanSource ./.;
  libraryHaskellDepends = [
    base base-prelude Chart chartfold constraints containers
    data-default-class lens text vector-space parallel
  ];
  homepage = "https://github.com/k0001/hs-chartfold";
  description = "Chart library backend for the chartfold library";
  license = lib.licenses.asl20;
}
