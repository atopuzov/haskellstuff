{
  ghcVersion ? "ghc8107"
}:
let
  sources = import ./nix/sources.nix;
  nixpkgs = sources.nixpkgs;
  pkgs = import nixpkgs {};
  compiler = pkgs.pkgs.haskell.packages.${ghcVersion};
  drv = compiler.callPackage ./bikes.nix { };
in
drv
