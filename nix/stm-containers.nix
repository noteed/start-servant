self: super:
let

  lib = super.lib;

  inherit (import ./sources.nix) gitignore stm-containers stm-hamt focus; 

  stmContainersOverrides = selfh: superh: {
    stm-containers =
      selfh.callCabal2nix "stm-containers" stm-containers { };
    # the following are the dependencies of stm-containers. 
    stm-hamt =
      selfh.callCabal2nix "stm-hamt" stm-hamt { };
    focus =
      selfh.callCabal2nix "focus" focus { };

  };

in {
  haskellPackages = super.haskellPackages.override (old: {
    overrides =
      lib.composeExtensions (old.overrides or (_: _: { })) stmContainersOverrides;
  });
}
