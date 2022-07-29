# Expose the host's NixOS configuration values for use in our Home Manager
# configuration.

{ ... }:

{
  _module.args = {
    nixos-config = (import <nixpkgs/nixos> {}).config;
  };
}
