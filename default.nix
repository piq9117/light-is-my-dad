{ mkDerivation, aeson, base, beam-core, beam-postgres, bytestring
, exceptions, lib, mtl, postgresql-simple, relude, resource-pool
, servant, servant-server, time, uuid, wai, warp
}:
mkDerivation {
  pname = "servant-login";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base beam-core beam-postgres bytestring exceptions mtl
    postgresql-simple relude resource-pool servant servant-server time
    uuid wai warp
  ];
  executableHaskellDepends = [ base warp ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
