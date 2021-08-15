# This file defines a Nix shell environment. It is normally used in conjunction
# with a `.envrc` file and the direnv tool. But it can also be manually
# activated by runnin `nix-shell` in this directory. Once activated, programs
# such as ghci will be in scope.

{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    buildInputs = [
      pkgs.ghcid
      (pkgs.haskellPackages.ghcWithPackages (hpkgs: [
        hpkgs.hlint
        hpkgs.servant
        hpkgs.servant-auth
        hpkgs.servant-auth-server
        hpkgs.servant-blaze
        hpkgs.servant-server
        hpkgs.wai
      ]))
    ];
}
