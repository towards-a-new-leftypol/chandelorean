{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellPackages = nixpkgs.haskellPackages;
  repa = haskellPackages.callCabal2nix "repa" ./repa/repa {};

  env = repa.env.overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [
      haskellPackages.cabal-install
    ];
  });

in
  if nixpkgs.lib.inNixShell then env else repa
