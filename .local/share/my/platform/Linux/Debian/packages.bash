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
                  [bash-language-server]=bash-language-server
                                  [bear]=bear
                       [build-essential]=build-essential
                                 [cargo]=cargo
                                 [clang]="$(_my_greatest_pkg clang)"
                                [clangd]="$(_my_greatest_pkg clangd)"
                     [command-not-found]=command-not-found
    [corrector-of-llvm-xray-stack-flame]=corrector_of_llvm_xray_stack_flame
                             [emacs-nox]=emacs-nox
                               [fd-find]=fd-find
                                 [flock]=util-linux
                                   [gcc]=gcc
                                   [gdb]=gdb
                                   [git]=git
                          [gnu-autoconf]=autoconf
                          [gnu-binutils]=binutils
                         [gnu-coreutils]=coreutils
                         [gnu-diffutils]=diffutils
                              [gnu-grep]=:  # Apparently, there are no APT packages for these (?).
                              [gnu-make]=make
                             [gnu-patch]=patch
                               [gnu-sed]=:
                               [gnu-tar]=:
                                 [gnupg]=gnupg
                                  [htop]=htop
                                  [lsof]=lsof
                             [man-pages]=manpages
                                  [most]=most
              [my-bash-history-combiner]=my_bash_history_combiner
                     [my-emacs-packages]=my-emacs-packages
                                  [nano]=nano
                                   [npm]=npm
                                [psmisc]=psmisc
                               [ripgrep]=ripgrep
                                  [rust]=rustc
                         [rust-analyzer]=rust-analyzer
                                [screen]=screen
                            [shellcheck]=shellcheck
                                   [wrk]=wrk
)

# Maps a platform-specific package name to its platform-specific command for installing it.  Each
# value (an eval'ed command) may be multiple words quoted (e.g. to pass options to a command).
#
readonly -A MY_PLATFORM_SPECIFIC_PACKAGES_METHODS=(
                                     [:]=true  # Do nothing
                              [autoconf]=my-apt-install
                       [bash-completion]=my-apt-install
                  [bash-language-server]=my-npm-install
                                  [bear]=my-apt-install
                              [binutils]=my-apt-install
                       [build-essential]=my-apt-install
                                 [cargo]=my-apt-install
                     [command-not-found]=my-apt-install
                             [coreutils]=my-apt-install
    [corrector_of_llvm_xray_stack_flame]=my-cargo-install-from-my-repo
                             [diffutils]=my-apt-install
                             [emacs-nox]=my-apt-install
                               [fd-find]=my-apt-install
                                   [gcc]=my-apt-install
                                   [gdb]=my-apt-install
                                   [git]=my-apt-install
                                 [gnupg]=my-apt-install
                                  [htop]=my-apt-install
                                  [lsof]=my-apt-install
                                  [make]=my-apt-install
                              [manpages]=my-apt-install
                                  [most]=my-apt-install
              [my_bash_history_combiner]=my-cargo-install-from-my-repo
                     [my-emacs-packages]=my-install-emacs-packages
                                  [nano]=my-apt-install
                                   [npm]=my-apt-install
                                 [patch]=my-apt-install
                                [psmisc]=my-apt-install
                               [ripgrep]=my-apt-install
                                 [rustc]=my-apt-install
                         [rust-analyzer]=my-rustup-install--rust-analyzer  # No APT package.
                                [screen]=my-apt-install
                            [shellcheck]=my-apt-install
                            [util-linux]=my-apt-install
                                   [wrk]=my-apt-install
          ["$(_my_greatest_pkg clangd)"]=my-apt-install
)

unset -f _my_greatest_pkg


function my-apt-install {
    sudo apt-get install --yes "$@"
}
