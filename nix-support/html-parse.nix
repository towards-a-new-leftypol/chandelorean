{ nixpkgs ? import <nixpkgs> {} }:

let
  src = nixpkgs.fetchFromGitHub {
    owner = "Zer0-";
    repo = "html-parse";
    rev = "master";
    sha256 = "sha256-CsGNpL+EWCdhWCe7me1l0LjyvAy8xm51UYwVYd4SI3g=";
  };

  drv = nixpkgs.haskellPackages.callCabal2nix "html-parse" src { };
in

  drv
