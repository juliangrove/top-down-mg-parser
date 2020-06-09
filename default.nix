{ mkDerivation, base, mtl, stdenv }:
mkDerivation {
  pname = "top-down-mg-parser";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base mtl ];
  homepage = "https://github.com/juliangrove/top-down-mg-parser#readme";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
