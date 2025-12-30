{
  description = "cc";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # Use default haskellPackages (latest supported GHC)
        # We can switch to pkgs.haskell.packages.ghcXYZ if needed.
        haskellPackages = pkgs.haskellPackages;
        
        packageName = "cc";
        
        # callCabal2nix automatically handles package.yaml
        project = haskellPackages.callCabal2nix packageName ./. {};
      in
      {
        packages.${packageName} = project;
        packages.default = self.packages.${system}.${packageName};

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            hpack
          ];
          
          # Include dependencies from the project
          inputsFrom = [ project.env ];
        };
      }
    );
}
