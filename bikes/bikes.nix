{ mkDerivation, aeson, base, bytestring, containers, geohash, hpack
, http-client, http-client-tls, http-conduit, influxdb, lens, mtl
, stdenv, text, time
}:
mkDerivation {
  pname = "bikes";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers geohash http-client
    http-client-tls http-conduit influxdb lens mtl text time
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring containers geohash http-client
    http-client-tls http-conduit influxdb lens mtl text time
  ];
  testHaskellDepends = [
    aeson base bytestring containers geohash http-client
    http-client-tls http-conduit influxdb lens mtl text time
  ];
  prePatch = "hpack";
  homepage = "https://github.com/atopuzov/haskellstuff#readme";
  license = stdenv.lib.licenses.bsd3;
}
