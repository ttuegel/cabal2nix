{ overlays ? [] }:

let
  readJSON = file: builtins.fromJSON (builtins.readFile file);
  inherit ((import <nixpkgs> {}).pkgs) fetchgit lib;
  bootstrap = fetchgit {
    inherit (readJSON ./nixpkgs.json) url rev sha256 fetchSubmodules;
  };
in

let
  importOverrides = file:
    if builtins.pathExists file
    then import file
    else self: super: {}
    ;
in

import bootstrap
{
  config.allowUnfree = true;
  overlays =
    [ (importOverrides ./default.overrides.nix) ]
    ++
    overlays
    ;
}
