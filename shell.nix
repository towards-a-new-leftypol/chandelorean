{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }: 
let

  inherit (nixpkgs) pkgs;

  perceptual-hash = import ./nix-support/perceptual-hash.nix { inherit nixpkgs; };
  http-conduit = import ./nix-support/http-conduit.nix { inherit nixpkgs; };

  f = { mkDerivation, base, stdenv, cabal-install,
        aeson, safe-exceptions, bytestring, cmdargs,
        http-conduit, cryptonite, memory, mime-types,
        perceptual-hash
      }:
      mkDerivation {
        pname = "chan-delorean";
        version = "0.0.2";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base safe-exceptions aeson bytestring cmdargs http-conduit
          cryptonite memory mime-types perceptual-hash
        ];
        testHaskellDepends = [ cabal-install ];
        license = "unknown";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {
    perceptual-hash = perceptual-hash;
    http-conduit = http-conduit.http-conduit;
  });

  enhancedDrv = if pkgs.lib.inNixShell
    then drv.env.overrideAttrs (oldAttrs: {
      buildInputs = oldAttrs.buildInputs or [] ++ [ pkgs.postgresql ];
    })
    else drv;

in

  enhancedDrv
