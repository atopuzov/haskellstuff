{ nixpkgs ? (import ./nixpkgs.nix) }:
let
  pkgs = import nixpkgs {};

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
in pkgs.stdenv.mkDerivation rec {
  name = "stack-nix";

  buildInputs = with pkgs; [
    stack
    haskell.compiler.ghc865
    zlib
    glpk
    pcre
    lzma
    zlib
    bzip2
  ];

  # if not present compilations fails with:
  # can't load .so/.DLL for: libz.so (libz.so: cannot open shared object file: No such file or directory)
  LD_LIBRARY_PATH = pkgs.stdenv.lib.makeLibraryPath buildInputs;

  shellHook = ''
    echo "Ready!"
  '';
}
