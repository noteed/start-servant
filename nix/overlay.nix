self: super:
let

  lib = super.lib;
  sources = import ./sources.nix;

  inherit (import sources.gitignore { inherit lib; }) gitignoreSource;

  ourOverrides = selfh: superh: {
    start-servant =
      selfh.callCabal2nix "start-servant" (gitignoreSource ../.) { };
  };

in {
  haskellPackages = super.haskellPackages.override (old: {
    overrides =
      lib.composeExtensions (old.overrides or (_: _: { })) ourOverrides;
  });
}
