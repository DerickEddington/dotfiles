{ pkgs, ... }:

{
  _module.args = {

    # Provide my own library of helpers.
    inherit (pkgs) myLib;  # Depends on my overlays adding it to `pkgs`.

    # Expose the host's NixOS configuration values for use in our Home Manager configuration.
    nixos-config = pkgs.myLib.nixos.config;
  };
}
