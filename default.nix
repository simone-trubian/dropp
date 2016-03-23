{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
let
  inherit (nixpkgs) pkgs;
  cabal = pkgs.cabal-install;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
    http-conduit
    html-conduit
    xml-conduit
    text
    haxl
    amazonka
    amazonka-ses
    hlint
  ]);
in
pkgs.stdenv.mkDerivation {
  name = "dropp";
  buildInputs = [
    cabal
    ghc
  ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
