# Pass defaults to my actual overlays definition.  This exists to adapt the standard Nixpkgs
# overlays config file (this file) to do it my way.  A user may modify or delete this file as
# desired.
#
# This file should only be used for operations like `nix-env` that exist independently and do not
# involve Home Manager, NixOS, etc.  For the overlays of a Nixpkgs instance used by Home Manager
# (etc), this file should not be used, and instead the `nixpkgs.overlays` option of H.M. (or
# similar) could be set to `import ~/${loc}/my/overlays ...` so that my overlays are passed
# arguments that are the same as used by H.M. (etc) and so that this file's defaults are not used.

import ./my/overlays (_self: _super: {
})
