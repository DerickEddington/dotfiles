# Use the same overlays as the NixOS system configuration and/or use
# user-specific overlays.  By default (as this file was initially provided),
# both are used, but a user may modify or delete this file as desired.

let
  inherit (builtins) tryEval isPath pathExists;
  isDir = path: pathExists (path + "/default.nix");
in

let
  systemWideOverlays = let
    nixos-config = (tryEval <nixos-config>).value;
    dir = if isDir nixos-config then nixos-config else dirOf nixos-config;
    file = dir + "/nixpkgs/overlays.nix";
  in
    if (isPath nixos-config) && (pathExists file) then import file else [];

  myOverlays = [
    # Add yours here:
  ];
in
(systemWideOverlays ++ myOverlays)
