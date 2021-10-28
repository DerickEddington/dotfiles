# TODO: Doesn't this file apply to all of my user's use of nixpkgs - not just
# home-manager?  Don't think I want that.  Try to figure out how to only make
# NUR active for my home.nix doing the installation of the Firefox addons.

# TODO: Is there some proper way to make this available to all users?  As needed
# by /etc/nixos/my-home-manager-for-all-users.nix
# Could the nixpkgs.overlays option of home-manager be used for this, and this
# would be defined in /etc/nixos/my-home-manager-for-all-users.nix so that it
# applies to all users.

{ pkgs, ... }:

let
  lib = pkgs.lib;
in
{
  # Note that `NIXPKGS_ALLOW_UNFREE=1 nix-env -qa` can be used to see all
  # "unfree" packages without allowing permanently.

  # Allow and show all "unfree" packages that are available.
  # allowUnfree = true;

  # Allow and show only select "unfree" packages.
  allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    # "${name}"
  ];

  packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
      inherit pkgs;
    };
  };
}
