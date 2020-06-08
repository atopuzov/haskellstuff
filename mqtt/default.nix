{
  ghcVersion ? "ghc865"
}:
let
  nixpkgs = import ./nixpkgs.nix;
  pkgs = import nixpkgs {};

  overrides = {
    overrides = self: super: {
      mqtt-hs = pkgs.haskell.lib.overrideCabal super.mqtt-hs (drv: {
        libraryHaskellDepends = drv.libraryHaskellDepends ++
                                [ pkgs.haskell.packages."${ghcVersion}".connection ];
        src = pkgs.fetchFromGitHub {
          owner = "atopuzov";
          repo = "mqtt-hs";
          rev = "e6d99d19d84b636775adb28e312a0ef8e2481896";
          sha256 = "1cglyp55jxlq87k32apkyfbz4farrlw7qxp5w905ql4ss3gffv1d";
        };
        broken = false;
      });
    };
  };
  compiler = pkgs.pkgs.haskell.packages.${ghcVersion}.override overrides;
  drv = compiler.callPackage ./mqtt.nix { };
in
drv
