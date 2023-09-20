# Defines the mapping of my package names to their platform-specific packages and how to install
# those.

# shellcheck disable=SC2034  # These variables are used by what `source`s this file.


source "$(dirname "${BASH_SOURCE[0]}")"/../../../bash/helpers.bash


# If already source'd, don't do anything.
_my_bash_sourced_already local/share/my/platform/Linux/Ubuntu/packages && return


function _my_clangd_greatest {
    apt-cache search --names-only clangd | grep -E -o '^clangd(-[0-9]+)?' | sort -V | tail -n1
}

# Maps my own convention of a package name to its platform-specific package name.
#
readonly -A MY_PLATFORM_SPECIFIC_PACKAGES_NAMES=(
             [bash-completion]=bash-completion
                       [cargo]=cargo
                      [clangd]="$(_my_clangd_greatest)"
                     [fd-find]=fd-find
                         [git]=git
               [gnu-coreutils]=coreutils
                        [htop]=htop
                        [most]=most
    [my-bash-history-combiner]=my_bash_history_combiner
                        [nano]=nano
                     [ripgrep]=ripgrep
                        [rust]=rustc
                      [screen]=screen
                  [util-linux]=util-linux
    # TODO: the others
)

# Maps a platform-specific package name to its platform-specific command for installing it.  Each
# value (an eval'ed command) may be multiple words quoted (e.g. to pass options to a command).
#
readonly -A MY_PLATFORM_SPECIFIC_PACKAGES_METHODS=(
             [bash-completion]=my-apt-install
                       [cargo]=my-apt-install
    ["$(_my_clangd_greatest)"]=my-apt-install
                   [coreutils]=my-apt-install
                     [fd-find]=my-apt-install
                         [git]=my-apt-install
                        [htop]=my-apt-install
                        [most]=my-apt-install
    [my_bash_history_combiner]="single my-cargo-install-user-local-from-my-repo"
                        [nano]=my-apt-install
                     [ripgrep]=my-apt-install
                       [rustc]=my-apt-install
                      [screen]=my-apt-install
                  [util-linux]=my-apt-install
    # TODO: the others
)

unset -f _my_clangd_greatest


function my-apt-install {
    sudo apt-get install --yes "$@"
}
