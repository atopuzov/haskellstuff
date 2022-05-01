{
  ghcVersion ? "ghc8107"
}:
let
  sources = import ./nix/sources.nix;
  nixpkgs = sources.nixpkgs;
  pkgs = import nixpkgs {};

  overrides = {
    overrides = self: super: {
      mqtt-hs = pkgs.haskell.lib.overrideCabal super.mqtt-hs (drv: {
        libraryHaskellDepends = drv.libraryHaskellDepends ++
                                [ pkgs.haskell.packages."${ghcVersion}".connection ];
        src = pkgs.fetchFromGitHub {
          owner = "atopuzov";
          repo = "mqtt-hs";
          rev = "05b8b688a03fb6e047348eeb8bdff91ba50dd7d6";
          sha256 = "sha256-AS+I1zYGpgxSRKRPukET1TGrf+wTNWZgqypzlicS0oM=";
        };
        broken = false;
      });
    };
  };
  compiler = pkgs.pkgs.haskell.packages.${ghcVersion}.override overrides;
  drv = compiler.callPackage ./mqtt.nix { };
in
drv
