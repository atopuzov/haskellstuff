{ mkDerivation, aeson, async, base, containers, hpack, influxdb
, lens, mqtt-hs, mtl, optparse-applicative, stdenv, stm, text, time
, uuid
}:
mkDerivation {
  pname = "mqtt";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base containers influxdb lens mqtt-hs mtl
    optparse-applicative stm text time uuid
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson async base containers influxdb lens mqtt-hs mtl
    optparse-applicative stm text time uuid
  ];
  testHaskellDepends = [
    aeson async base containers influxdb lens mqtt-hs mtl
    optparse-applicative stm text time uuid
  ];
  prePatch = "hpack";
  homepage = "https://github.com/atopuzov/mqtt#readme";
  license = stdenv.lib.licenses.bsd3;
}
