{ mkDerivation, base, hpack, mtl, QuickCheck, quickcheck-classes
, stdenv, transformers
}:
mkDerivation {
  pname = "baastad";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base mtl quickcheck-classes transformers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base mtl quickcheck-classes transformers
  ];
  testHaskellDepends = [
    base mtl QuickCheck quickcheck-classes transformers
  ];
  prePatch = "hpack";
  homepage = "https://github.com/atopuzov/baastad#readme";
  license = stdenv.lib.licenses.bsd3;
}
