{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
let
  inherit (nixpkgs) pkgs;
  cabal = pkgs.cabal-install;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
    hlint
    wreq
    resourcet
    haxl
  ]);
in
pkgs.stdenv.mkDerivation {
  name = "dropp-haxl";
  buildInputs = [
    cabal
    ghc
  ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
