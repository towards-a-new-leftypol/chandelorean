{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }: 
let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, stdenv, cabal-install,
        aeson, safe-exceptions, bytestring, cmdargs
      }:
      mkDerivation {
        pname = "chan-delorean";
        version = "0.0.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base safe-exceptions aeson bytestring cmdargs
        ];
        testHaskellDepends = [ cabal-install ];
        license = "unknown";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

  enhancedDrv = if pkgs.lib.inNixShell
    then drv.env.overrideAttrs (oldAttrs: {
      buildInputs = oldAttrs.buildInputs or [] ++ [ pkgs.postgresql ];
    })
    else drv;

in

  enhancedDrv
