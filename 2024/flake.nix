{
  description = "A very basic flake, for AoC2024 with Haskell";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
  outputs = { self, nixpkgs }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in {
      devShell.x86_64-linux = pkgs.mkShell {
        buildInputs = with (pkgs.haskellPackages);
          [ ghc haskell-language-server ghcid ];
      };
  };
}
