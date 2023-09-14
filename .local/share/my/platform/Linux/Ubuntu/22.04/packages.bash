# Defines the mapping of my package names to their platform-specific packages and how to install
# those.

# shellcheck disable=SC2034  # These variables are used by what `source`s this file.


source "$(dirname "${BASH_SOURCE[0]}")"/../../../../bash/helpers.bash


# If already source'd, don't do anything.
_my_bash_sourced_already local/share/my/platform/Linux/Ubuntu/22.04/packages && return


# Maps my own convention of a package name to its platform-specific package name.
#
readonly -A MY_PLATFORM_SPECIFIC_PACKAGES_NAMES=(
                       [cargo]=TODO
                      [clangd]=clangd-15
                     [fd-find]=fd-find
                         [git]=git
                        [htop]=htop
                        [most]=most
    [my-bash-history-combiner]=my_bash_history_combiner
                     [ripgrep]=ripgrep
                        [rust]=TODO
                      [screen]=screen
    # TODO: the others
)

# Maps a platform-specific package name to its platform-specific command for installing it.  Each
# value (an eval'ed command) may be multiple words quoted (e.g. to pass options to a command).
#
readonly -A MY_PLATFORM_SPECIFIC_PACKAGES_METHODS=(
                       [cargo]=TODO
                   [clangd-15]=my-apt-install
                     [fd-find]=my-apt-install
                         [git]=my-apt-install
                        [htop]=my-apt-install
                        [most]=my-apt-install
    [my_bash_history_combiner]="single my-cargo-install-user-local-from-my-repo"
                     [ripgrep]=my-apt-install
                        [rust]=TODO
                      [screen]=my-apt-install
    # TODO: the others
)


function my-apt-install {
    sudo apt-get install --yes "$@"
}
