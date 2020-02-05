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
          rev = "fb0400aae07224c9a957404e8290a213641c2b62";
          sha256 = "1zr76994654h9zffn57wrq8ijfnv0ix7pyrq617yv9sgdr4rvmq3";
        };
        broken = false;
      });
    };
  };
  haskellPackages = pkgs.haskell.packages.${compiler}.override overrides;
  drv = haskellPackages.callPackage ./mqtt.nix { };
in
if pkgs.lib.inNixShell then drv.env else drv
