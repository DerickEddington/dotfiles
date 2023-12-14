# Defines the mapping of my package names to their platform-specific packages and how to install
# those.

# shellcheck disable=SC2034  # These variables are used by what `source`s this file.


# If already source'd, don't do anything.
_my_bash_sourced_already local/share/my/platform/Linux/Alpine/packages && return


function _my_greatest_pkg {
    apk list "*${1:?}*"                 \
    | std cut -d' ' -f3                 \
    | gnu sed -E -e 's/\{(.*)\}/\1/'    \
    | gnu sort -V                       \
    | std tail -n 1
}

clangVer=$(_my_greatest_pkg clang)


# Maps my own convention of a package name to its platform-specific package name.
#
readonly -A MY_PLATFORM_SPECIFIC_PACKAGES_NAMES=(
                       [bash-completion]=bash-completion
                  [bash-language-server]=bash-language-server
                                  [bear]=bear
                       [build-essential]=build-essential
                                 [cargo]=cargo
                                 [clang]="$clangVer"
                                [clangd]="$clangVer"-extra-tools
                     [command-not-found]=command-not-found
    [corrector-of-llvm-xray-stack-flame]=corrector_of_llvm_xray_stack_flame
                             [emacs-nox]=emacs-nox
                               [fd-find]=fd
                                 [flock]=util-linux
                                   [gcc]=gcc
                                   [gdb]=gdb
                                   [git]=git
                          [gnu-autoconf]=autoconf
                          [gnu-binutils]=binutils
                         [gnu-coreutils]=coreutils
                         [gnu-diffutils]=diffutils
                              [gnu-grep]=grep
                              [gnu-make]=make
                             [gnu-patch]=patch
                               [gnu-sed]=sed
                               [gnu-tar]=tar
                                 [gnupg]=gnupg
                                  [htop]=htop
                                  [lsof]=lsof
                                [man-db]=man-db
                             [man-pages]=man-pages
                                  [most]=most
              [my-bash-history-combiner]=my_bash_history_combiner
                     [my-emacs-packages]=my-emacs-packages
                                  [nano]=nano
                                   [npm]=npm
                                [psmisc]=psmisc
                               [ripgrep]=ripgrep
                                  [rust]=rust
                         [rust-analyzer]=rust-analyzer
                           [rust-clippy]=rust-clippy
                              [rust-gdb]=rust-gdb
                                [screen]=screen
                            [shellcheck]=shellcheck
                                  [tput]=ncurses
                                [tzdata]=tzdata
                         [lib/slang/dev]=slang-dev
                                   [wrk]=wrk
)

# Maps a platform-specific package name to its platform-specific command for installing it.  Each
# value (an eval'ed command) may be multiple words quoted (e.g. to pass options to a command).
#
readonly -A MY_PLATFORM_SPECIFIC_PACKAGES_METHODS=(
                                     [:]=true  # Do nothing
                              [autoconf]=my-apk-install
                       [bash-completion]=my-apk-install
                  [bash-language-server]=my-npm-install
                                  [bear]=my-apk-install
                              [binutils]=my-apk-install
                       [build-essential]=_my-install-build-essential
                                 [cargo]=my-apk-install
                           ["$clangVer"]=my-apk-install
               ["$clangVer"-extra-tools]=my-apk-install
                     [command-not-found]=my-apk-install
                             [coreutils]=my-apk-install
    [corrector_of_llvm_xray_stack_flame]=my-cargo-install-from-my-repo
                             [diffutils]=my-apk-install
                             [emacs-nox]=my-apk-install
                                    [fd]=my-apk-install
                                   [gcc]=my-apk-install
                                   [gdb]=my-apk-install
                                   [git]=my-apk-install
                                  [grep]=my-apk-install
                                 [gnupg]=my-apk-install
                                  [htop]=my-apk-install
                                  [lsof]=my-apk-install
                                  [make]=my-apk-install
                                [man-db]=my-apk-install
                             [man-pages]=my-apk-install
                                  [most]=my-jedsoft-build-and-install
              [my_bash_history_combiner]=my-cargo-install-from-my-repo
                     [my-emacs-packages]=my-install-emacs-packages
                                  [nano]=my-apk-install
                               [ncurses]=my-apk-install
                                   [npm]=my-apk-install
                                 [patch]=my-apk-install
                                [psmisc]=my-apk-install
                               [ripgrep]=my-apk-install
                                  [rust]=my-apk-install
                         [rust-analyzer]='my-deps-install rust-clippy -- my-apk-install'
                           [rust-clippy]=my-apk-install
                              [rust-gdb]=my-apk-install
                                [screen]=my-apk-install
                                   [sed]=my-apk-install
                            [shellcheck]=my-apk-install
                             [slang-dev]=my-apk-install
                                   [tar]=my-apk-install
                                [tzdata]=my-apk-install
                            [util-linux]=my-apk-install
                                   [wrk]=my-apk-install
)

unset clangVer


function my-apk-install {
    sudo apk add "$@"
}


function _my-install-build-essential {
    (( $# == 1 )) && [ "${1:?}" = build-essential ] || exit
    my-apk-install autoconf automake binutils gcc make patch
}
