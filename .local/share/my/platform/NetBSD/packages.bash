# Defines the mapping of my package names to their platform-specific packages and how to install
# those.

# shellcheck disable=SC2034  # These variables are used by what `source`s this file.


# If already source'd, don't do anything.
_my_bash_sourced_already local/share/my/platform/NetBSD/packages && return


# Maps my own convention of a package name to its platform-specific package name.
#
readonly -A MY_PLATFORM_SPECIFIC_PACKAGES_NAMES=(
                       [bash-completion]=bash-completion
                  [bash-language-server]=bash-language-server
                                  [bear]=bear
                       [build-essential]=build-essential
                                 [cargo]=rust
                                 [clang]=clang
                                [clangd]=clang-tools-extra
                                 [cmake]=cmake
                     [command-not-found]=""  # Or does some package actually provide it?
    [corrector-of-llvm-xray-stack-flame]=corrector_of_llvm_xray_stack_flame
                             [emacs-nox]=emacs-nox11
                               [etc-ssl]=etc-ssl
                               [fd-find]=fd-find
                            [fs-monitor]=glib2  # For `gio monitor`.
                                 [flock]=:  # NetBSD already has this "out of the box".
                                   [gcc]=:  # ditto
                                   [gdb]=:  # ditto
                                   [git]=git
                          [gnu-autoconf]=autoconf
                          [gnu-binutils]=binutils
                         [gnu-coreutils]=coreutils
                         [gnu-diffutils]=diffutils
                              [gnu-grep]=grep
                              [gnu-make]=gmake
                             [gnu-patch]=patch
                               [gnu-sed]=gsed
                               [gnu-tar]=gtar
                                 [gnupg]=gnupg2
                                [gnutls]=gnutls
                                  [gRPC]=grpc
                                  [htop]=htop
                 [lib/c++/nlohmann-json]=nlohmann-json
                                  [lsof]=lsof
                                  [most]=most
             [mozilla-rootcerts-openssl]=mozilla-rootcerts-openssl
              [my-bash-history-combiner]=my_bash_history_combiner
                     [my-emacs-packages]=my-emacs-packages
                                  [nano]=nano
                                 [ninja]=ninja-build
                                   [npm]=nodejs
                         [p5-Mozilla-CA]=p5-Mozilla-CA
                              [protobuf]=protobuf
                                [psmisc]=psmisc
                               [ripgrep]=ripgrep
                                  [rust]=rust
                         [rust-analyzer]=rust
                                [screen]=screen
                            [shellcheck]=shellcheck
)

# Maps a platform-specific package name to its platform-specific command for installing it.  Each
# value (an eval'ed command) may be multiple words quoted (e.g. to pass options to a command).
#
readonly -A MY_PLATFORM_SPECIFIC_PACKAGES_METHODS=(
                                     [:]=true  # Do nothing
                              [autoconf]=my-pkgin-install
                       [bash-completion]=my-pkgin-install
                  [bash-language-server]=my-npm-install
                                  [bear]=my-build-and-install--bear
                              [binutils]=my-pkgin-install
                       [build-essential]=_my-install--build-essential
                                 [clang]=my-pkgin-install
                     [clang-tools-extra]=my-pkgin-install
                                 [cmake]=my-pkgin-install
                             [coreutils]=my-pkgin-install
    [corrector_of_llvm_xray_stack_flame]=my-cargo-install-from-my-repo
                             [diffutils]=my-pkgin-install
                           [emacs-nox11]='my-deps-install etc-ssl gnutls -- my-pkgin-install'
                               [etc-ssl]=_my-create--etc-ssl
                               [fd-find]=my-pkgin-install
                                   [gcc]=my-pkgin-install
                                   [gdb]=my-pkgin-install
                                   [git]=my-pkgin-install
                                 [glib2]=my-pkgin-install
                                 [gmake]=my-pkgin-install
                                [gnupg2]=my-pkgin-install
                                [gnutls]=my-pkgin-install
                                  [grep]=my-pkgin-install
                                  [grpc]=my-pkgin-install
                                  [gsed]=my-pkgin-install
                                  [gtar]=my-pkgin-install
                                  [htop]=my-pkgin-install
                                  [lsof]=my-pkgin-install
                                  [most]=my-pkgin-install
             [mozilla-rootcerts-openssl]=my-pkgin-install
              [my_bash_history_combiner]=my-cargo-install-from-my-repo
                     [my-emacs-packages]=my-install-emacs-packages
                                  [nano]=my-pkgin-install
                           [ninja-build]=my-pkgin-install
                         [nlohmann-json]=my-pkgin-install
                                [nodejs]=my-pkgin-install
                         [p5-Mozilla-CA]=my-pkgin-install
                                 [patch]=my-pkgin-install
                              [protobuf]=my-pkgin-install
                                [psmisc]=my-pkgin-install
                               [ripgrep]=my-pkgin-install
                                  [rust]='my-deps-install mozilla-rootcerts-openssl \
                                            -- my-pkgin-install'
                                [screen]=my-pkgin-install
                            [shellcheck]=my-pkgin-install
)


function my-pkgin-install {
    sudo pkgin -y install "$@"
}

function is-pkg-installed {
    (( $# == 1 )) || exit
    pkg_info -q -E "${1:?}" > /dev/null
}

function _my-install--build-essential {
    (( $# == 1 )) && [ "${1:?}" = build-essential ] || exit
    my-pkgin-install autoconf automake pkg-config
    # NetBSD already has: gcc linker make patch
}

function _my-create--etc-ssl {
    # Some of NetBSD's own packages, e.g. its Emacs, assume that /etc/ssl/ exists, but NetBSD's
    # location is /etc/openssl/.  So we create a symlink to that for compatibility.
    if ! [ -e /etc/ssl ]; then
        sudo ln -s openssl /etc/ssl
    fi
}


# Install GNU `coreutils` immediately, because it's used for installing other packages.
if ! is-pkg-installed coreutils ; then
    my-pkgin-install coreutils || warn "Couldn't install GNU coreutils. Hopefully it already is."
fi
