{ nixpkgs ? (import ./nixpkgs.nix) }:
# You may need to manually run `unset STACK_IN_NIX_SHELL`
# (doesn't work from a `shellHook`; see: https://github.com/commercialhaskell/stack/issues/5000
let
  pkgs = import nixpkgs {};
  oldGhcSrc = builtins.fetchGit {
    url = "https://github.com/mpickering/old-ghc-nix";
    rev = "674d459c7376af6641c94e91cd7d36214661b481";
    ref = "master";
  };

  oldGhc = import oldGhcSrc { inherit pkgs; };

  ghc = oldGhc.ghc822;
in
with pkgs;
haskell.lib.buildStackProject {
  inherit ghc;
  name = "stack-nix-shell";

  # System dependencies used at build-time go in here.
  nativeBuildInputs = [ ghc ];
  # System dependencies used at run-time go in here.
  buildInputs = [ zlib ghc ];

  src = if lib.inNixShell then null else ./.;
}
