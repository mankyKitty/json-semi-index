{ mkDerivation, base, bytestring, hw-bits, hw-prim
, hw-rankselect-base, lens, stdenv, vector
}:
mkDerivation {
  pname = "json-semi-index";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring hw-bits hw-prim hw-rankselect-base lens vector
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
