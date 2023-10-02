# Use the same overlays as the system-wide NixOS configuration and/or use user-specific overlays.
# By default (as this file was initially provided), both are used, but a user may modify or delete
# this file as desired.

# Add your choices to these:

let
  # Packages to override to have debugging support added.  This causes rebuilding of these.
  debuggingSupportFor = pkgs: {
    inherit (pkgs)
      my-hello-test  # Have this to always exercise my Nix library for debugging support.
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
    # Add my altered "hello" package.  Can be useful for testing.  This is in `firstOverlays` so
    # that `super.hello` is not the overridden package from `systemWideOverlays` (that package
    # usually adds debugging support in its own way, but we want to ensure that only our way is in
    # effect here).  We do not use `self.hello` because that usually is the one from
    # `systemWideOverlays`.
    (self: super:
      if early.myLib ? makeHelloTestPkg then {
        my-hello-test = early.myLib.makeHelloTestPkg super;
      } else {})
  ];

  systemWideOverlays = let
    pathName = early.myLib.nixosConfigLoc.dirName + "/nixpkgs/overlays.nix";
  in
    if early.myLib.nixosConfigLoc.isDefined && (builtins.pathExists pathName)
    then import pathName deps
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
      if myLib ? pkgWithDebuggingSupport then
        (myLib.pkgWithDebuggingSupport.byMyConfig debuggingSupportConfig).overlayResult selection
      else {})
  ];

  choicesOverlays = customOverlays ++ debuggingSupportOverlays;
in

firstOverlays ++ systemWideOverlays ++ myLibOverlays ++ choicesOverlays
