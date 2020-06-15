{ mkDerivation, base, hpack, mtl, QuickCheck
, quickcheck-classes-base, stdenv, transformers
}:
mkDerivation {
  pname = "baastad";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base mtl transformers ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base mtl transformers ];
  testHaskellDepends = [
    base mtl QuickCheck quickcheck-classes-base transformers
  ];
  prePatch = "hpack";
  homepage = "https://github.com/atopuzov/baastad#readme";
  license = stdenv.lib.licenses.bsd3;
}
