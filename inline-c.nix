{ mkDerivation, ansi-wl-pprint, base, binary, bytestring
, containers, cryptohash, directory, filepath, gsl, gslcblas, hspec
, mtl, parsec, parsers, QuickCheck, raw-strings-qq, regex-posix
, stdenv, template-haskell, transformers, unordered-containers
, vector
}:
mkDerivation {
  pname = "inline-c";
  version = "0.5.0.0";
  sha256 = "08j051xzxsgix9pahap2ww0rc1m9lxqr3xhjxgrfsbwpmsc9ij94";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    ansi-wl-pprint base binary bytestring containers cryptohash
    directory filepath mtl parsec parsers QuickCheck template-haskell
    transformers unordered-containers vector
  ];
  testDepends = [
    ansi-wl-pprint base containers hspec parsers QuickCheck
    raw-strings-qq regex-posix template-haskell transformers vector
  ];
  extraLibraries = [ gsl gslcblas ];
  description = "Write Haskell source files including C code inline. No FFI required.";
  license = stdenv.lib.licenses.mit;
}
