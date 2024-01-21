{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellPackages = nixpkgs.haskellPackages;

  src = nixpkgs.fetchFromGitHub {
    owner = "towards-a-new-leftypol";
    repo = "http-client";
    rev = "a32d92fff9171a8beb948b430e274a1667b3ca35";
    sha256 = "sha256-LfBTsB2fHZ1z+wvt3mowowXL+Ta7jesj+L3Y8NqSmfI=";
  };

  http-client = haskellPackages.callCabal2nix "http-client" (src + "/http-client") {};

  conduitPackages = nixpkgs.haskellPackages.override {
    overrides = self: super: {
      http-client = http-client;
    };
  };

  http-conduit = conduitPackages.callCabal2nix "http-conduit" (src + "/http-conduit") {};

  env = http-client.env.overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [
      haskellPackages.cabal-install
    ];
  });
in

  {
    http-conduit = http-conduit;
    http-client = http-client;
    env = env;
  }

