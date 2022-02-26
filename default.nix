let overlays = import ./nix/overlays.nix;
    sources = import ./nix/sources.nix; 
in (import sources.nixpkgs { inherit overlays; }).haskellPackages.start-servant
