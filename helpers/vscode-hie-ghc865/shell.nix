let
  pkgs = import (import ./nixpkgs.nix) {};

  # Haskell IDE engine
  allHiesSrc = builtins.fetchGit {
    url = "https://github.com/atopuzov/all-hies/";
    rev = "f8249043a53f0f8e5f5415c0b47a28e00fcf5558";
    ref = "nixos-20.03";
  };

  allHies = import allHiesSrc { inherit pkgs; };

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
  haskell = pkgs.haskell.packages.ghc865.ghcWithPackages (pkgs: with pkgs; [
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
