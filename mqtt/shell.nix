{
  ghcVersion ? "ghc8107"
}:
let
  sources = import ./nix/sources.nix;
  nixpkgs = sources.nixpkgs;
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
