# For bootstrapping my setup independently of my other more-involved package-installing modules.
#
_my_install_git() {
    nix-env --install --attr nixos.git
    # Lower its priority (by increasing the numeric value), so Git can later be installed via the
    # user's Home Manager configuration.
    nix-env --set-flag priority 10 git
}
