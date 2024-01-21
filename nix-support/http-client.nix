{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellPackages = nixpkgs.haskellPackages;
  http-client = haskellPackages.callCabal2nix "http-client" (nixpkgs.fetchFromGitHub {
    owner = "towards-a-new-leftypol";
    repo = "http-client";
    rev = "7900de1687fe1ec244abf9801009d27a619a3ba0";
    sha256 = "";
  } + "/http-client") {};
in
  http-client

