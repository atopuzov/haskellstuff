{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc844" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./mqtt.nix { }
