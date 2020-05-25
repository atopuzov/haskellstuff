let
  oldPkgs = import (import ./nixpkgs.nix) {}; # Nixpkgs is pinned to the same version as all-hies for ghc865
  pkgs = import <nixpkgs> {};

  # Haskell IDE engine
  allHiesSrc = builtins.fetchGit {
    url = "https://github.com/Infinisil/all-hies/";
    rev = "4b6aab017cdf96a90641dc287437685675d598da";
    ref = "master";
  };
  allHies = import allHiesSrc { pkgs = oldPkgs; };

  hie = allHies.selection { selector = p: { inherit (p) ghc865; }; };

  stack = pkgs.stdenv.mkDerivation {
    name = "stack-system-ghc";
    builder = pkgs.writeScript "stack" ''
      source $stdenv/setup
      mkdir -p $out/bin
      makeWrapper ${pkgs.stack}/bin/stack \
        $out/bin/stack \
        --add-flags "--system-ghc --no-nix --no-docker"
    '';
    buildInputs = [ pkgs.makeWrapper ];
  };

  # GHC with tools and packges
  haskell = oldPkgs.haskell.packages.ghc865.ghcWithPackages (pkgs: with pkgs; [
    # language tools
    stylish-haskell
    hindent
    hlint
    hoogle
    cabal-install
  ]);

  # Visual studio code
  vscode = pkgs.vscode.overrideDerivation (old: rec {
    postFixup = ''
      wrapProgram \
        $out/bin/code \
        --prefix PATH : ${pkgs.lib.makeBinPath [haskell hie stack]}
    '';
  });
in
pkgs.mkShell {
  buildInputs = [
    haskell
    stack
    hie
    vscode
    pkgs.zlib
  ];

  shellHook = ''
    echo "Ready!"
    # code --folder-uri $PWD
  '';
}
