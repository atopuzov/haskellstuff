{
  ghcVersion ? "ghc865"
}:
let
  nixpkgs = import ./nixpkgs.nix;
  pkgs = import nixpkgs {};
  drv = import ./default.nix { };
  compiler = pkgs.pkgs.haskell.packages.${ghcVersion};
in
drv.env.overrideAttrs (shellEnv: {
  buildInputs = shellEnv.buildInputs ++ [
    pkgs.cabal2nix
    pkgs.cabal-install
    compiler.hpack
  ];

  shellHook = ''
    echo "Ready!"
  '';
})
