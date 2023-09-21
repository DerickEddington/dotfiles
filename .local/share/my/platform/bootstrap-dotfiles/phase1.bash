set -o errexit -o nounset
shopt -s assoc_expand_once extglob
(( "${VERBOSE:=0}" >= 3 )) && set -o xtrace
(( "${VERBOSE:=0}" >= 4 )) && set -o verbose


# TODO: invoke install-desired-packages, or something
#       Doing this first has the advantage that Git should be installed by it (if not already),
#       before needing to use Git next for setting-up the ~/.dotfiles repo.


# TODO: Move below to some new setup-user executable script, or something

# Make user's login shell be Bash, if not already.
# Hopefully, this is portable enough? (https://en.wikipedia.org/wiki/Chsh)
#
sudo chsh -s "$(command -v bash)" "$(logname)" || warn "Failed to change login shell to Bash."


# TODO: All of what my-remote-setup old version did, but as running in the target remote host.
