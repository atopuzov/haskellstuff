{ mkDerivation, ansi-terminal, base, directory, hpack, mtl
, optparse-applicative, stdenv
}:
mkDerivation {
  pname = "myls";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    ansi-terminal base directory mtl optparse-applicative
  ];
  testHaskellDepends = [ base ];
  prePatch = "hpack";
  homepage = "https://github.com/atopuzov/haskellstuff#readme";
  license = stdenv.lib.licenses.bsd3;
}
