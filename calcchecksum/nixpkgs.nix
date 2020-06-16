# git ls-remote https://github.com/NixOS/nixpkgs-channels nixos-20.03
builtins.fetchGit {
  url = "https://github.com/NixOS/nixpkgs-channels";
  rev = "db31e48c5c8d99dcaf4e5883a96181f6ac4ad6f6";
  ref = "nixos-20.03";
}
