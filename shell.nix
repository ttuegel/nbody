with (import <nixpkgs> {});
with haskell-ng.lib;
with haskellPackages;
let inline-c = dontCheck (callPackage ./inline-c.nix { gslcblas = null; });
in (callPackage ./. { inherit inline-c; }).env
