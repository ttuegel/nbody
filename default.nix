{ mkDerivation, base, inline-c, stdenv, vector }:
mkDerivation {
  pname = "nbody";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ base inline-c vector ];
  description = "nbody examples from benchmarksgame.alioth.debian.org";
  license = stdenv.lib.licenses.bsd3;
}
