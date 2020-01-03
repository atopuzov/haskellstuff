{ pkgs ? import <nixpkgs> {}, compiler ? "ghc865"
}:
let
  overrides = {
    overrides = self: super: {
      mqtt-hs = pkgs.haskell.lib.overrideCabal super.mqtt-hs (drv: {
        src = pkgs.fetchFromGitHub {
          owner = "atopuzov";
          repo = "mqtt-hs";
          rev = "5eb90ddbbddf14c6a02e5a2257a058a8c36b3731";
          sha256 = "0c1c3mjsh36cpsyg4hisqzhxd3sglidmnmizqjg19swzp5kwf7p8";
        };
        broken = false;
      });
    };
  };
  haskellPackages = pkgs.haskell.packages.${compiler}.override overrides;
  drv = haskellPackages.callPackage ./mqtt.nix { };
in
if pkgs.lib.inNixShell then drv.env else drv
