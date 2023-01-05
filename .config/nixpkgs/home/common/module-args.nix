{ pkgs, lib, ... }:

let
  workTreeTop = ../../../..;
in
{
  _module.args = {

    # Expose the host's NixOS configuration values for use in our Home Manager configuration.
    nixos-config = (import <nixpkgs/nixos> {}).config;

    # Provide my own library of helpers.
    myLib = import (workTreeTop + "/.config/nixpkgs/my/lib") { inherit pkgs lib; };
  };
}
