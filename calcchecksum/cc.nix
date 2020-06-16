{ mkDerivation, base, hpack, stdenv }:
mkDerivation {
  pname = "cc";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  prePatch = "hpack";
  homepage = "https://github.com/atopuzov/haskellstuff#readme";
  license = stdenv.lib.licenses.bsd3;
}
