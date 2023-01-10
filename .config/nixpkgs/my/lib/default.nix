# My own library of helpers.  The dependencies (the arguments to this expression-file's function)
# may be given as `null`, to support limited uses of the few parts of this library where those are
# not needed.  This file may be `import`ed by other arbitrary uses independently of the Home
# Manager configuration evaluation or of the Nixpkgs evaluation.

{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib or null, sysWideMyLib ? null }:

let
  scope = rec {
    propagate = { inherit pkgs lib; myLib = both; };

    sys.myLib =
      if sysWideMyLib != null then
        sysWideMyLib  # Get it as provided by system-wide overlay.
      else if user.myLib.nixosConfigLoc.isDefined then
        import (user.myLib.nixosConfigLoc.dirName + "/lib") { inherit pkgs lib; }
      else
        null;

    user.myLib = {
      # Have what is provided by the other myLib from the host's NixOS-configuration directory.
      inherit sys;

      # Have what is defined by this directory.
      nixosConfigLoc                    = import ./nixos-configuration-location.nix;
      nixos                             = import ./nixos-evaluated-value.nix;
      # another                         = import ./whatever.nix                         propagate;
    };

    # Have both.  Allow shadowing of sys.myLib.
    both = (if sys.myLib != null then sys.myLib else {}) // user.myLib;
  };
in

scope.both
