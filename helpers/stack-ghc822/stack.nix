# You may need to manually run `unset STACK_IN_NIX_SHELL`
# (doesn't work from a `shellHook`; see: https://github.com/commercialhaskell/stack/issues/5000

with import <nixos> {};
let
  nurSrc = builtins.fetchTarball {
    # Get the revision by choosing a version from https://github.com/nix-community/NUR/commits/master
    url = "https://github.com/nix-community/NUR/archive/4178c02f17191018b491dde88217138665aa030d.tar.gz";
    # Get the hash by running `nix-prefetch-url --unpack <url>` on the above url
    sha256 = "1niwm6mrcgn5ka9xmjv48qv418w1ds5mwib7nqa8ania7wc8rvp0";
  };

  nur = import nurSrc {
    inherit pkgs;
  };

  ghc = nur.repos.mpickering.ghc.ghc822;

in

haskell.lib.buildStackProject {
  inherit ghc;
  name = "stack-nix-shell";

  # System dependencies used at build-time go in here.
  buildInputs = [ zlib ];
  # System dependencies used at run-time go in here.
  buildInputs = [ ];

  src = if lib.inNixShell then null else ./.;
}
