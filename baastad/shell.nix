{
  ghcVersion ? "ghc8107"
}:
let
  sources = import ./nix/sources.nix;
  nixpkgs = sources.nixpkgs;
  pkgs = import nixpkgs {};
  compiler = pkgs.pkgs.haskell.packages.${ghcVersion};
  drv = compiler.callPackage ./baastad.nix { };
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
