{ config, pkgs, lib, ... }:

let
  inherit (builtins) concatMap;
  inherit (lib) mkEnableOption mkIf mkOption types;

  cfg = config.my.python;
in

{
  options.my.python = with types; {
    enable = mkEnableOption "installation of Python with my choices";

    package = mkOption {
      description = "Which Python runtime to install.";
      type = package;
      default = pkgs.python3;
    };

    extraPackages = mkOption {
      description = "Which Python modules to install.";
      type = listOf (functionTo (listOf package));
      default = [];
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      (cfg.package.withPackages (pypkgs: concatMap (f: f pypkgs) cfg.extraPackages))
    ];
  };
}
