{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellPackages = nixpkgs.haskellPackages.override {
    overrides = self: super: {
      repa = import ./repa.nix { inherit nixpkgs; };
    };
  };

  hip = haskellPackages.callCabal2nix "hip" (nixpkgs.fetchFromGitHub {
    owner = "lehins";
    repo = "hip";
    rev = "9f1111ea8e3f6d284404074cb6ac3e2ff164f0fe";
    sha256 = "sha256-/GhspMhU5MjFu7B6AU4tkaqqNGpR3daoOJpuatHemlM=";
  }) {};
in
  hip
