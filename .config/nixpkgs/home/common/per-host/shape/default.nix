# Options common to all users but specific to this particular host machine.

{
  my = {
    # This DPI corresponds to my current external monitor which is a Philips 346B.
    dpi = 110;
    # Defining services.xserver.dpi (in /etc/nixos/configuration.nix) confused
    # Firefox and degraded its UI, independently of its other DPI settings below.
    # dpi = nixpkgs.config.services.xserver.dpi;
  };
}
