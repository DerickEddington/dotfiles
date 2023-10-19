# For NixOS, assume that Home Manager should be used.  As such, this just delegates to that, which
# uses ~/.config/home-manager/home.nix to specify which packages to install, instead of using the
# specification in ~/.config/my/platform/config.bash and instead of the definitions here.  As
# such, the definitions here are dummies, but they do cause the execution of `home-manager`, in
# order to apply the user's Home Manager configuration to the $HOME, in order to install whatever
# packages that specifies.

# shellcheck disable=SC2034  # These variables are used by what `source`s this file.


# If already source'd, don't do anything.
_my_bash_sourced_already local/share/my/platform/Linux/NixOS/packages && return


# Maps my own convention of a package name to the dummy package name.
#
readonly -A MY_PLATFORM_SPECIFIC_PACKAGES_NAMES=(
                       [bash-completion]=dummy
                  [bash-language-server]=dummy
                                  [bear]=dummy
                       [build-essential]=dummy
                                 [cargo]=dummy
                                 [clang]=dummy
                                [clangd]=dummy
                     [command-not-found]=dummy
    [corrector-of-llvm-xray-stack-flame]=dummy
                             [emacs-nox]=dummy
                               [fd-find]=dummy
                                 [flock]=dummy
                                   [gcc]=dummy
                                   [gdb]=dummy
                                   [git]=dummy
                          [gnu-autoconf]=dummy
                          [gnu-binutils]=dummy
                         [gnu-coreutils]=dummy
                         [gnu-diffutils]=dummy
                              [gnu-grep]=dummy
                              [gnu-make]=dummy
                             [gnu-patch]=dummy
                               [gnu-sed]=dummy
                               [gnu-tar]=dummy
                                 [gnupg]=dummy
                                  [htop]=dummy
                                  [lsof]=dummy
                                  [most]=dummy
              [my-bash-history-combiner]=dummy
                                  [nano]=dummy
                                   [npm]=dummy
                                [psmisc]=dummy
                               [ripgrep]=dummy
                                  [rust]=dummy
                         [rust-analyzer]=dummy
                                [screen]=dummy
                            [shellcheck]=dummy
                                   [wrk]=dummy
)

# Maps the dummy package name to the platform-specific command for installing Home Manager along
# with whatever packages ~/.config/home-manager/home.nix specifies.  Each value (an eval'ed
# command) may be multiple words quoted (e.g. to pass options to a command).
#
readonly -A MY_PLATFORM_SPECIFIC_PACKAGES_METHODS=(
    [dummy]=my-home-manager-install
)


function my-home-manager-install {
    # This assumes that the `home-manager` channel was already setup for the target $HOME by
    # ~/.config/my/deploy-setup/hooks/prepare-home (typically by
    # ~/.local/share/my/deploy-setup/bootstrap/start1.bash).
    #
    NIX_PATH=${HOME:?}/.nix-defexpr/channels${NIX_PATH:+:}${NIX_PATH-} \
        nix-shell '<home-manager>' -A install

    # # Dump its news now so it will be quiet about this after.
    # home-manager news > /dev/null
}
