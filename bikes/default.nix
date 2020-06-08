{
  ghcVersion ? "ghc865"
}:
let
  nixpkgs = import ./nixpkgs.nix;
  pkgs = import nixpkgs {};
  compiler = pkgs.pkgs.haskell.packages.${ghcVersion};
  drv = compiler.callPackage ./bikes.nix { };
in
drv
