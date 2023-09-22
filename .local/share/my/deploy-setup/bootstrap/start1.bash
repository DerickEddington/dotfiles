set -o errexit -o nounset
shopt -s assoc_expand_once extglob
(( "${VERBOSE:=0}" >= 3 )) && set -o xtrace
(( "${VERBOSE:=0}" >= 4 )) && set -o verbose

# Capture arguments, before anything else could mess with them.
#
readonly args=("$@")
readonly primaryDotfilesBranch="${args[0]}"

# Might as well.
readonly XDG_DATA_HOME XDG_CONFIG_HOME XDG_RUNTIME_DIR XDG_STATE_HOME XDG_CACHE_HOME

# shellcheck source=../../bash/helpers.bash
source "${XDG_DATA_HOME:?}"/my/bash/helpers.bash



# TODO: invoke install-desired-packages, or something
#       Doing this first has the advantage that Git should be installed by it (if not already),
#       before needing to use Git next for setting-up the ~/.dotfiles repo.


# TODO: Move below to some new setup-user executable script, or something

# Make user's login shell be Bash, if not already.
# Hopefully, this is portable enough? (https://en.wikipedia.org/wiki/Chsh)
#
sudo chsh -s "$(command -v bash)" "$(logname)" || warn "Failed to change login shell to Bash."


# TODO: All of what my-remote-setup old version did, but as running in the target host.
