# Options common to all users but specific to this particular host machine.

{ lib, ... }:

let
  inherit (lib) mkDefault;
in

{
  # Have debug-info and source-code for packages where this is applied.  This is for packages that
  # normally don't provide these, and this uses my custom approach that overrides and overlays
  # packages to achieve having these.
  my.debugging.support.all.enable = mkDefault true;

  programs.firefox.profiles.default.settings = {
    # Needed with Firefox 103+ for the font sizes to look right with the 110 DPI
    # of my Philips 346B monitor.  (Note that this replaces
    # `layout.css.devPixelsPerPx = 1.4` since that now would screw-up the UI
    # element sizes with Firefox 103+.)
    "ui.textScaleFactor" = 140;
  };
}
