{ pkgs ? import <nixpkgs> {}}:
let
  project = pkgs.haskellPackages.callPackage ./default.nix { };
  ghc922 = pkgs.haskell.compiler.ghc922;
in
pkgs.mkShell {
  name = "servant-login";
  buildInputs = with pkgs; [
    zlib
    cabal-install
    haskell.compiler.ghc922
    ghcid
    ormolu
    dbmate
    postgresql
  ];
  LD_LIBRARY_PATH="${pkgs.zlib.outPath}/lib";
  DATABASE_URL="postgres://servant-login:password@127.0.0.1:5432/servant-login?sslmode=disable";
}
