# Nixpkgs is pinned to the same version as all-hies for ghc865
{ nixpkgs ? (import ./nixpkgs.nix) }:
let
  pkgs = import nixpkgs {};

  # Haskell IDE engine
  allHiesSrc = builtins.fetchGit {
    url = "https://github.com/Infinisil/all-hies/";
    rev = "4b6aab017cdf96a90641dc287437685675d598da";
    ref = "master";
  };
  allHies = import allHiesSrc { inherit pkgs; };

  hie = allHies.selection { selector = p: { inherit (p) ghc865; }; };

  # GHC with tools and packges
  haskell = pkgs.haskell.packages.ghc865.ghcWithPackages (pkgs: with pkgs; [

    # language tools
    stylish-haskell
    hindent
    hlint
    hoogle
    cabal-install
    stack
  ]);

  # Visual studio code
  vscode = pkgs.vscode.overrideDerivation (old: rec {
    version = "1.45.0";
    name = "vscode-${version}";
    src = pkgs.fetchurl rec {
      name = "VSCode_${version}_linux-x64.tar.gz";
      url = "https://vscode-update.azurewebsites.net/${version}/linux-x64/stable";
      sha256 = "16zchjp72m6n6za4ak5kn2ax1s5pjfn7l082d6gfbb2y62isvs7q";
    };
    postFixup = ''
      wrapProgram \
        $out/bin/code \
        --prefix PATH : ${pkgs.lib.makeBinPath [haskell hie]}
      '';
  });
in
pkgs.mkShell {
  buildInputs = [
    haskell
    hie
    vscode
  ];

  shellHook = ''
    echo "Ready!"
    # code --folder-uri $PWD
  '';
}
