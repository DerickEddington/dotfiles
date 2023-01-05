# My own library of helpers.

{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib }:

let
  # Have what is defined by this directory.
  myUserLib = {
    nixosConfigLoc = import ./nixosConfigLoc.nix;
  };

  inherit (myUserLib) nixosConfigLoc;

  # Have what is defined by the host's NixOS-configuration directory.
  mySysLib =
    if nixosConfigLoc.isDefined
    then import (nixosConfigLoc.dirName + "/lib") { inherit pkgs lib; }
    else {};
in

# Have both.  Allow myUserLib to shadow mySysLib.
mySysLib // myUserLib
