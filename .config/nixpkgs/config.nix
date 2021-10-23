# TODO: Doesn't this file apply to all of my user's use of nixpkgs - not just
# home-manager?  Don't think I want that.  Try to figure out how to only make
# NUR active for my home.nix doing the installation of the Firefox addons.

# TODO: Is there some proper way to make this available to all users?  As needed
# by /etc/nixos/my-home-manager-for-all-users.nix
# Could the nixpkgs.overlays option of home-manager be used for this, and this
# would be defined in /etc/nixos/my-home-manager-for-all-users.nix so that it
# applies to all users.

{
  packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
      inherit pkgs;
    };
  };
}
