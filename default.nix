{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
let
  inherit (nixpkgs) pkgs;

  # Please note that Sqitch requires PosgreSQL 9.4 which is present in
  # configuration.nix
  sqitch = pkgs.sqitchPg;

  # Haskell.
  cabal = pkgs.cabal-install;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
    http-conduit
    html-conduit
    xml-conduit
    text
    haxl
    amazonka
    amazonka-ses
    servant
    servant-server
    servant-lucid
    wai
    warp
    yaml

    # Testing
    hlint
    tasty
    tasty-hunit

    # DB
    postgrest
  ]);
in
pkgs.stdenv.mkDerivation {
  name = "dropp";
  version = "0.3.2.1";
  buildInputs = [
    cabal
    ghc
    sqitch
  ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
