{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "demo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}
