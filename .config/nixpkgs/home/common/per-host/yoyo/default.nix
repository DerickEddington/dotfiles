# Options common to all users but specific to this particular host machine.

{
  # Automatically install the "debug" output of packages if they have one, and
  # set the NIX_DEBUG_INFO_DIRS environment variable to include them, for GDB to
  # find them.
  home.enableDebugInfo = true;

  programs.firefox.profiles.default.settings = {
    # Needed with Firefox 103+ for the font sizes to look right with the 110 DPI
    # of my Philips 346B monitor.  (Note that this replaces
    # `layout.css.devPixelsPerPx = 1.4` since that now would screw-up the UI
    # element sizes with Firefox 103+.)
    "ui.textScaleFactor" = 140;
  };
}
