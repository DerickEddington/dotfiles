# Defines the mapping of my package names to their platform-specific packages and how to install
# those.

# shellcheck disable=SC2034  # These variables are used by what `source`s this file.


# If already source'd, don't do anything.
_my_bash_sourced_already local/share/my/platform/Linux/Debian/packages && return


function _my_greatest_pkg {
    apt-cache search --names-only "^${1:?}(-[0-9]+)?$"  \
    | gnu egrep --only-matching "^${1}(-[0-9]+)?"       \
    | gnu sort --version-sort                           \
    | std tail -n 1
}

# Maps my own convention of a package name to its platform-specific package name.
#
readonly -A MY_PLATFORM_SPECIFIC_PACKAGES_NAMES=(
             [bash-completion]=bash-completion
                        [bear]=bear
                       [cargo]=cargo
                       [clang]="$(_my_greatest_pkg clang)"
                      [clangd]="$(_my_greatest_pkg clangd)"
                     [fd-find]=fd-find
                         [gcc]=gcc
                         [git]=git
               [gnu-coreutils]=coreutils
                    [gnu-grep]=:  # Apparently, there are no APT packages for these (?).
                     [gnu-sed]=:
                     [gnu-tar]=:
                       [gnupg]=gnupg
                        [htop]=htop
                        [most]=most
    [my-bash-history-combiner]=my_bash_history_combiner
                        [nano]=nano
                     [ripgrep]=ripgrep
                        [rust]=rustc
                      [screen]=screen
                  [util-linux]=util-linux
)

# Maps a platform-specific package name to its platform-specific command for installing it.  Each
# value (an eval'ed command) may be multiple words quoted (e.g. to pass options to a command).
#
readonly -A MY_PLATFORM_SPECIFIC_PACKAGES_METHODS=(
                               [:]=true  # Do nothing
                 [bash-completion]=my-apt-install
                            [bear]=my-apt-install
                           [cargo]=my-apt-install
     ["$(_my_greatest_pkg clang)"]=my-apt-install
    ["$(_my_greatest_pkg clangd)"]=my-apt-install
                       [coreutils]=my-apt-install
                         [fd-find]=my-apt-install
                             [gcc]=my-apt-install
                             [git]=my-apt-install
                           [gnupg]=my-apt-install
                            [htop]=my-apt-install
                            [most]=my-apt-install
        [my_bash_history_combiner]="single my-cargo-install-user-local-from-my-repo"
                            [nano]=my-apt-install
                         [ripgrep]=my-apt-install
                           [rustc]=my-apt-install
                          [screen]=my-apt-install
                      [util-linux]=my-apt-install
)

unset -f _my_greatest_pkg


function my-apt-install {
    sudo apt-get install --yes "$@"
}
