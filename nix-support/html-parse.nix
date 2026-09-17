{ nixpkgs ? import <nixpkgs> {} }:

let
  src = nixpkgs.fetchFromGitHub {
    owner = "Zer0-";
    repo = "html-parse";
    rev = "master";
    sha256 = "sha256-/HqfX2sHZofdxLntD7+Sj2VKVz6m+D+DxuUM45KggsU=";
  };

  drv = nixpkgs.haskellPackages.callCabal2nix "html-parse" src { };
in

  drv
