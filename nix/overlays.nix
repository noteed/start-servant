# Central overlay that supplies all overlays that:
# 1. Make this package available.
# 2. Provide this particular package with a fixed point of overlayed packages, if they become needed.
let

  sources = import ./sources.nix;

  # We can overlay haskell packages here.
  haskellOverlays = let
    # stm-containers is marked as broken in nixpkgs; so we're building it ourselves.
    stm-containers-overlay = import ./stm-containers.nix;
    design-hs-overlay = import "${sources.design-hs}/nix/overlay.nix";

  in with sources; [ stm-containers-overlay design-hs-overlay ];

in haskellOverlays ++ [ (import ./overlay.nix) ]
