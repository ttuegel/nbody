{ mkDerivation, base, stdenv, vector }:
mkDerivation {
  pname = "nbody";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ base vector ];
  license = stdenv.lib.licenses.bsd3;
}
