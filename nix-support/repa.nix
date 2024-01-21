{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellPackages = nixpkgs.haskellPackages;
  repa = haskellPackages.callCabal2nix "repa" (nixpkgs.fetchFromGitHub {
    owner = "towards-a-new-leftypol";
    repo = "repa";
    rev = "8d30d271d0dc87f39fdc8dd6d330a9c59174892e";
    sha256 = "sha256-ECx7YhOFHQDy+/qlMCcM+EHud/r/3jVTPeWXVLUOawE=";
  } + "/repa") {};
in
  repa

