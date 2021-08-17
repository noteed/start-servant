let

  sources = import ./nix/sources.nix;
  overlays = import ./nix/overlays.nix;

  # Overlays let us override certain packages at a central location.
  nixpkgs-overlayed = import sources.nixpkgs { inherit overlays; };
  nixpkgs = import sources.nixpkgs { };
  hp = nixpkgs-overlayed.haskellPackages;

  # Brittany, as the formatter, is just here as an example.
  # I personally prefer to have the formatters pinned to a version and then
  # made available via direnv to avoid unnecessary diff pollution across upgrades.
  brittany = hp.callCabal2nix "brittany" sources.brittany { };

  # Niv is great at pinning dependencies in sources.json and computing SHA's etc.
  nix-tooling = with hp; [ niv ];

  # Haskell tools
  haskell-tooling = with hp; [ cabal-install ghcid hlint ];

  # Add more as we need them.
  formatters = [ brittany ];

in hp.shellFor {
  packages = p: with p; [ start-servant ];
  buildInputs = nix-tooling ++ haskell-tooling ++ formatters;
}
