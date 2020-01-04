{ pkgs ? import <nixpkgs> {}, compiler ? "ghc865"
}:

pkgs.pkgs.haskell.packages.${compiler}.callPackage ./bikes.nix { }
