let nixpkgs_source = (fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-21.05.tar.gz);
in
{ pkgs ? import nixpkgs_source {
    inherit system;
  }
, system ? builtins.currentSystem
}:
let
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; ([
    base
    mtl
  ]));

in
pkgs.stdenv.mkDerivation {
  name = "top-down-mg-parser";

  src = pkgs.fetchFromGitHub {
    owner = "juliangrove";
    repo = "top-down-mg-parser";
    rev = "8704b28f5d11b8b0af5b8816262c3b639e7397b1";
    sha256 = "sha256-H8mh8gG0hIVwcmp21prDCDS+GLMK0Uya1iBjGC09jmM=";
  };

  buildPhase = ''
    cd src
    ghc -c Exprs.hs GornTrees.hs Lexer.hs Read_lexicon.hs MG_parser.hs -package mtl
    ghc -o top-down-mg-parser Main.hs
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp top-down-mg-parser $out/bin
  '';

  buildInputs = [
    ghc
  ];
}
