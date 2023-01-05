# Use the same overlays as the NixOS system configuration and/or use
# user-specific overlays.  By default (as this file was initially provided),
# both are used, but a user may modify or delete this file as desired.

let
  inherit (builtins) pathExists;
  myLib = import ./my/lib {};
  inherit (myLib) nixosConfigLoc;
in

let
  systemWideOverlays = let
    file = nixosConfigLoc.dirName + "/nixpkgs/overlays.nix";
  in
    if (nixosConfigLoc.isDefined) && (pathExists file) then import file else [];

  myOverlays = [
    # Add yours here:
  ];
in
(systemWideOverlays ++ myOverlays)
