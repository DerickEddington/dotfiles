# Remember that, instead of using this file, Home Manager can define the
# `nixpkgs.config` option.  Both ways are per-user, but the Home Manager option
# applies only within itself, and does not apply to `nix-env`.

{ pkgs, ... }:

let
  inherit (builtins) elem;
  inherit (pkgs.lib) getName;
in
{
  # Note that `NIXPKGS_ALLOW_UNFREE=1 nix-env -qa` can be used to see all
  # "unfree" packages without allowing permanently.

  # Allow and show all "unfree" packages that are available.
  # allowUnfree = true;

  # Allow and show only select "unfree" packages.
  allowUnfreePredicate = pkg: elem (getName pkg) [
    "vagrant"  # Vagrant 2.4+ has a different license than previous versions, it seems.
    # "${name}"
  ];

  # # Undesired since this would expose NUR (with all its untrusted packages)
  # # permanently.  Instead, fetch and import it only where used selectively.
  # # Uncommenting this temporarily might be useful for seeing listings like:
  # #   nix-env -f '<nixpkgs>' -qaP -A nur.repos.rycee.firefox-addons
  # packageOverrides = pkgs: {
  #   nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
  #     inherit pkgs;
  #   };
  # };
}
