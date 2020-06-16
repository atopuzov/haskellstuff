{
  ghcVersion ? "ghc865"
}:
let
  nixpkgs = import ./nixpkgs.nix;
  pkgs = import nixpkgs {};
  compiler = pkgs.pkgs.haskell.packages.${ghcVersion};
  drv = compiler.callPackage ./cc.nix { };
in
drv.env.overrideAttrs (shellEnv: {
  buildInputs = shellEnv.buildInputs ++ [
    pkgs.cabal2nix
    pkgs.cabal-install
  ];

  shellHook = ''
    echo "Ready!"
  '';
})
