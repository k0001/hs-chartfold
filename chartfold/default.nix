{ mkDerivation, base, base-prelude, colour, constraints, containers
, lens, lib, text, time, vector-space, mtl-prelude, parallel
}:
mkDerivation {
  pname = "chartfold";
  version = "0.1";
  src = lib.sources.cleanSource ./.;
  libraryHaskellDepends = [
    base base-prelude colour constraints containers lens text time
    vector-space mtl-prelude parallel
  ];
  homepage = "https://github.com/k0001/hs-chartfold";
  description = "Streaming charts";
  license = lib.licenses.asl20;
}
