{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
let
  inherit (nixpkgs) pkgs;
  cabal = pkgs.cabal-install;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
    hlint
    http-conduit
    html-conduit
    xml-conduit
    haxl
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
