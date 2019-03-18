{ pkgs ? import <nixpkgs> {}
}:

let
  stack-pkgs = pkgs.callPackage ./nix {};
in
  stack-pkgs.mqtt
