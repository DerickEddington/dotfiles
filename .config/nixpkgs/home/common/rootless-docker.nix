{ config, lib, nixos-config, ... }:

let
  inherit (lib) mkIf mkOption types;

  cfg = config.my.rootlessDocker;
in

{
  options.my.rootlessDocker = {
    autoStart = mkOption {
      description = ''
        Enable automatic starting of the \"rootless\" docker.service when our user logs-in.
        Only needed when /etc/nixos/configuration.nix has
          `systemd.user.services.docker.wantedBy == []`
        which makes it not auto-start for all users by default,
        and so a user that wants it to auto-start must choose so individually with this option.
      '';
      type = types.bool;
      default = false;
    };
  };

  config = {
    assertions = [{
      assertion = cfg.autoStart -> nixos-config.virtualisation.docker.rootless.enable == true;
      message = "The service is not enabled by the system-wide configuration.";
    } {
      assertion = cfg.autoStart -> nixos-config.systemd.user.services.docker.wantedBy == [];
      message = "The service is not unstarted by default.";
    }];

    # Note: Remember to use `home.username` if this ever changes to need to know
    # the user's name.
    systemd.user = mkIf cfg.autoStart {
      targets = {
        rootless-docker-auto-start = {
          Unit = {
            Description = "Start the (system-defined) \"rootless\" docker.service when our user logs-in.";
            Requires = "docker.service";
          };
          Install = {
            WantedBy = [ "default.target" ];
          };
        };
      };
    };
  };
}
