{ nixpkgs ? import <nixpkgs> {} }:

let
  # haskellPackages = nixpkgs.haskellPackages.override {
  #   overrides = self: super: {
  #     hip = import ./hip.nix { inherit nixpkgs; };
  #   };
  # };

  haskellPackages = nixpkgs.haskellPackages.override {
    overrides = self: super: {
      hip = nixpkgs.pkgs.haskell.lib.overrideCabal super.hip (drv: {
        configureFlags = (drv.configureFlags or [ ]) ++ [
          "--ghc-options=-fsimpl-tick-factor=200"
        ];
        broken = false;
      });
    };
  };

  perceptual-hash = haskellPackages.callCabal2nix "perceptual-hash" (nixpkgs.fetchFromGitHub {
    owner = "vmchale";
    repo = "phash";
    rev = "90fa59bec93b6adcad614d37033c1f2e059b6e46";
    sha256 = "sha256-CysvgcgzIuvKFEwNnCYHB6MXGzQnBQN7Nuo6HADuavk=";
  }) {};
in
  perceptual-hash
