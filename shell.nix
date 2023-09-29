{ pkgs ? import <nixpkgs> {} }:

let
  shell = pkgs.mkShell {
    buildInputs = with pkgs;
      [
        python3
        postgresql
        #python3Packages.regex
      ];
  };

in

  shell
