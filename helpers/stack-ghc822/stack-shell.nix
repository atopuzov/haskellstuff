with import <nixos> {};
let
  ghc = haskell.compiler.ghc822Binary;
in
haskell.lib.buildStackProject {
  name = "stack-nix-shell";
  buildInputs = [ ghc zlib ];
}
