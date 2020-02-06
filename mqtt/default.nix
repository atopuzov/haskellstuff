{ pkgs ? import <nixpkgs> {}, compiler ? "ghc865"
}:
let
  overrides = {
    overrides = self: super: {
      mqtt-hs = pkgs.haskell.lib.overrideCabal super.mqtt-hs (drv: {
        libraryHaskellDepends = drv.libraryHaskellDepends ++
                                [ pkgs.haskell.packages."${compiler}".connection ];
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
  haskellPackages = pkgs.haskell.packages.${compiler}.override overrides;
  drv = haskellPackages.callPackage ./mqtt.nix { };
in
if pkgs.lib.inNixShell then drv.env else drv
