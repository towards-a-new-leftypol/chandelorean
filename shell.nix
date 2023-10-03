{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }: 
let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, stdenv, cabal-install,
        aeson
      }:
      mkDerivation {
        pname = "chan-delorean";
        version = "0.0.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base safe-exceptions aeson
        ];
        testHaskellDepends = [ cabal-install ];
        license = "unknown";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {
    req = req;
  });

in

  if pkgs.lib.inNixShell then drv.env else drv
