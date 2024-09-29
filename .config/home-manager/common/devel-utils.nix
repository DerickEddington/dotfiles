{ config, pkgs, lib, ... }:

let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.my.devel-utils;
in
{
  options.my.devel-utils.enable = mkEnableOption "installation of my utils for development work";

  config = mkIf cfg.enable {
    home.packages = [
      (import ../../nixpkgs/my/noctty.nix { inherit pkgs; })
    ];
  };
}
