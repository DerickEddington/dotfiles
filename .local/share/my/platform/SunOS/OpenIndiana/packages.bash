# Defines the mapping of my package names to their platform-specific packages and how to install
# those.

# shellcheck disable=SC2034  # These variables are used by what `source`s this file.


# If already source'd, don't do anything.
_my_bash_sourced_already local/share/my/platform/SunOS/OpenIndiana/packages && return


# Maps my own convention of a package name to its platform-specific package name.
#
readonly -A MY_PLATFORM_SPECIFIC_PACKAGES_NAMES=(
                       [bash-completion]=bash-completion
                  [bash-language-server]=bash-language-server
                                  [bear]=bear
                       [build-essential]=build-essential
                                 [cargo]=rustc
                                 [clang]=build-essential  # Includes latest version, it seems.
                                [clangd]=clangd
                                 [cmake]=cmake
                     [command-not-found]=""  # TODO: Or does some package actually provide it?
    [corrector-of-llvm-xray-stack-flame]=corrector_of_llvm_xray_stack_flame
                             [emacs-nox]=gnu-emacs-no-x11
                               [fd-find]=fd-find
                                 [flock]=flock
                                   [gcc]=build-essential  # Includes version used by system.
                                [gcc-13]=gcc-13
                                   [gdb]=gdb
                                   [git]=git
                          [gnu-autoconf]=autoconf
                          [gnu-binutils]=gnu-binutils
                         [gnu-coreutils]=gnu-coreutils
                         [gnu-diffutils]=gnu-diffutils
                              [gnu-grep]=gnu-grep
                              [gnu-make]=gnu-make
                             [gnu-patch]=gnu-patch
                               [gnu-sed]=gnu-sed
                               [gnu-tar]=gnu-tar
                                 [gnupg]=gnupg
                                  [gRPC]=gRPC
                                  [htop]=htop
                                  [lsof]=lsof
                          [lib/c/c-ares]=libcares
                        [lib/c++/abseil]=abseil-cpp
                           [lib/c++/fmt]=fmt
                 [lib/c++/nlohmann-json]=nlohmann-json
                           [lib/c++/re2]=re2
                        [lib/c++/spdlog]=spdlog
                             [lib/pcre2]=pcre2
                         [lib/slang/dev]=slang
                                  [most]=most
              [my-bash-history-combiner]=my_bash_history_combiner
                     [my-emacs-packages]=my-emacs-packages
                                  [nano]=nano
                                   [npm]=nodejs-18  # The LTS version.
                                 [ninja]=ninja
                               [openssl]=openssl
                              [protobuf]=protobuf
                                [psmisc]=""  # TODO?
                               [ripgrep]=ripgrep
                                  [rust]=rustc
                         [rust-analyzer]=rustc
                                [screen]=screen
                            [shellcheck]=""  # OI has no Haskell-support packages, it seems.
                                   [wrk]=CANDO
)

# Maps a platform-specific package name to its platform-specific command for installing it.  Each
# value (an eval'ed command) may be multiple words quoted (e.g. to pass options to a command).
#
readonly -A MY_PLATFORM_SPECIFIC_PACKAGES_METHODS=(
                            [abseil-cpp]=my-pkg-install
                              [autoconf]=my-pkg-install
                       [bash-completion]=my-pkg-install
                  [bash-language-server]=my-npm-install
                                  [bear]=my-build-and-install--bear
                       [build-essential]=my-pkg-install
                                [clangd]=my-build-and-install--clangd
                                 [cmake]=my-pkg-install
    [corrector_of_llvm_xray_stack_flame]=my-cargo-install-from-my-repo
                               [fd-find]=my-cargo-install--fd-find
                                 [flock]=my-build-and-install--flock
                                   [fmt]=my-pkg-install
                                [gcc-13]=my-pkg-install
                                   [gdb]=my-pkg-install
                                   [git]=my-pkg-install
                          [gnu-binutils]=my-pkg-install
                         [gnu-coreutils]=my-pkg-install
                         [gnu-diffutils]=my-pkg-install
                      [gnu-emacs-no-x11]=my-pkg-install
                              [gnu-grep]=my-pkg-install
                                 [gnupg]=my-pkg-install
                              [gnu-make]=my-pkg-install
                             [gnu-patch]=my-pkg-install
                               [gnu-sed]=my-pkg-install
                               [gnu-tar]=my-pkg-install
                                  [gRPC]=my-build-and-install--gRPC
                                  [htop]=my-pkg-install
                              [libcares]=my-pkg-install
                                  [lsof]=my-pkg-install
                                  [most]=my-jedsoft-build-and-install
              [my_bash_history_combiner]=my-cargo-install-from-my-repo
                     [my-emacs-packages]=my-install-emacs-packages
                                  [nano]=my-pkg-install
                         [nlohmann-json]=my-pkg-install
                             [nodejs-18]=my-pkg-install
                                 [ninja]=my-pkg-install
                               [openssl]=my-pkg-install
                                 [pcre2]=my-pkg-install
                              [protobuf]=my-pkg-install
                                [psmisc]=my-pkg-install
                                   [re2]=my-pkg-install
                               [ripgrep]=my-cargo-install--ripgrep
                                 [rustc]='my-deps-install gcc-13 -- my-pkg-install'
                                [screen]=my-pkg-install
                                 [slang]=my-pkg-install
                                [spdlog]=my-pkg-install
)


function my-pkg-install {
    sudo pkg install --no-refresh "$@" || [ $? -eq 4 ]
}
