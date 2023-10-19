{ config, pkgs, lib, ... }:

let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.my.vagrant;
in
{
  options.my.vagrant.enable = mkEnableOption "my setup for Vagrant";

  config = let
    user = config.home.username;
  in
    mkIf cfg.enable {

      home.packages = with pkgs; [ vagrant ];

      my.emacs.extraPackages = [(epkgs: with epkgs; [ vagrant-tramp ])];

      # Customize the location of the large dispensable ~/.vagrant.d/boxes/ to be outside of the
      # home directory, so its contents are excluded from backups.  Note: it's still possible for
      # a user to reorganize this sub-dir to be located somewhere else some other way, and these
      # tmpfiles rules will not disturb it when it already exists.
      systemd.user.tmpfiles.rules = [
        "q /mnt/omit/%h                    0700 ${user} users -"
        "d /mnt/omit/%h/.vagrant.d         -    ${user} users -"
        "d /mnt/omit/%h/.vagrant.d/boxes   -    -       -     -"
        "d %h/.vagrant.d       - - - -"
        "L %h/.vagrant.d/boxes - - - - /mnt/omit%h/.vagrant.d/boxes"
      ];
    };
}
