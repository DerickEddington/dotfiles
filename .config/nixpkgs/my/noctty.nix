{ pkgs ? import <nixpkgs> {} }:

let
  inherit (pkgs) fetchFromGitHub stdenv;
in

stdenv.mkDerivation rec {
  pname = "noctty";
  version = "0.0.2";

  src = fetchFromGitHub {
    owner = "DerickEddington";
    repo = pname;
    rev = version;
    hash = "sha256-WqZG+40IbjTU0Fgp8YxlAnJX2pqN3LfMPUwucctmPXM=";
  };

  buildPhase = ''
    sh ./build.sh ${version}
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp ./noctty ./unused-terminal $out/bin/
  '';
}
