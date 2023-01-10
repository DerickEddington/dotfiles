# Use the same overlays as the system-wide NixOS configuration and/or use user-specific overlays.
# By default (as this file was initially provided), both are used, but a user may modify or delete
# this file as desired.

# Add your choices to these:

let
  # Packages to override to have debugging support added.  This causes rebuilding of these.
  debuggingSupportFor = pkgs: {
    inherit (pkgs)
    ;
  };

  # Whatever overlays you want.  The system-wide overlays precede these and so the `super` given
  # to these has those applied.  The debugging-support overlays follow these and so
  # `debuggingSupportFor` may refer to the results of these.
  customOverlays = [
    # (self: super: ...)
  ];
in


# The below is moreso internals, but you may modify it.

deps:  # `deps` is a function that returns dependencies for here, given a `self` and `super` pair.

let
  early.myLib = import ../lib { pkgs = null; };  # Only for use before `myLibOverlays`.

  firstOverlays = [
  ];

  systemWideOverlays =
    if early.myLib.nixosConfigLoc.isDefined
    then import (early.myLib.nixosConfigLoc.dirName + "/nixpkgs/overlays.nix") deps
    else [];

  myLibOverlays = [
    # Provide my own library of helpers that extends the system-wide myLib.  We must pass
    # `super.myLib` as `sysWideMyLib` because `self.myLib` would be wrong (because it is our own
    # result).
    (self: super: {
      myLib = import ../lib { pkgs = self; sysWideMyLib = super.myLib or null; };
    })
  ];

  debuggingSupportOverlays = [
    (self: super: let
      inherit (super) myLib;
      inherit (deps self super) debuggingSupportConfig;

      selection = debuggingSupportFor super;
    in
      (myLib.pkgWithDebuggingSupport.byMyConfig debuggingSupportConfig).overlayResult selection)
  ];

  choicesOverlays = customOverlays ++ debuggingSupportOverlays;
in

firstOverlays ++ systemWideOverlays ++ myLibOverlays ++ choicesOverlays
