# Defines the mapping of my package names to their platform-specific packages and how to install
# those.

# shellcheck disable=SC2034  # These variables are used by what `source`s this file.


# If already source'd, don't do anything.
_my_bash_sourced_already local/share/my/platform/FreeBSD/packages && return


function _my_llvm_greatest {
    pkg search llvm | grep -E -o '^llvm[0-9]+' | uniq | sort -V | tail -n1
}

# Maps my own convention of a package name to its platform-specific package name.
#
readonly -A MY_PLATFORM_SPECIFIC_PACKAGES_NAMES=(
                       [bash-completion]=bash-completion
                  [bash-language-server]=bash-language-server
                       [build-essential]=:  # FreeBSD already has these "out of the box".
                                  [bear]=bear
                                 [cargo]=rust
                                 [clang]="$(_my_llvm_greatest)"
                                [clangd]="$(_my_llvm_greatest)"
                     [command-not-found]=""  # Or does some package actually provide it?
    [corrector-of-llvm-xray-stack-flame]=corrector_of_llvm_xray_stack_flame
                             [emacs-nox]=emacs-nox
                               [fd-find]=fd-find
                                 [flock]=devel/util-linux
                                   [gcc]=gcc
                                   [gdb]=gdb
                                 [groff]=groff
                                   [git]=git
                          [gnu-autoconf]=autoconf
                          [gnu-binutils]=binutils
                         [gnu-coreutils]=coreutils
                         [gnu-diffutils]=diffutils
                              [gnu-grep]=gnugrep
                              [gnu-make]=gmake
                             [gnu-patch]=patch
                               [gnu-sed]=gsed
                               [gnu-tar]=gtar
                                 [gnupg]=gnupg
                                  [htop]=htop
                                  [lsof]=lsof
                                  [most]=most
              [my-bash-history-combiner]=my_bash_history_combiner
                     [my-emacs-packages]=my-emacs-packages
                                  [nano]=nano
                                   [npm]=npm  # Seems to stay at the LTS version of Node.js.
                          [pkg-provides]=pkg-provides
                                [psmisc]=psmisc
                               [ripgrep]=ripgrep
                                  [rust]=rust
                         [rust-analyzer]=rust-analyzer
                                [screen]=screen
                            [shellcheck]=hs-ShellCheck
                           [terminfo-db]=terminfo-db
                                   [wrk]=wrk-luajit
)

# Maps a platform-specific package name to its platform-specific command for installing it.  Each
# value (an eval'ed command) may be multiple words quoted (e.g. to pass options to a command).
#
readonly -A MY_PLATFORM_SPECIFIC_PACKAGES_METHODS=(
                                     [:]=true  # Do nothing
                              [autoconf]=my-pkg-install
                       [bash-completion]=my-pkg-install
                  [bash-language-server]=my-npm-install
                                  [bear]=my-pkg-install
                              [binutils]=my-pkg-install
                             [coreutils]=my-pkg-install
                ["$(_my_llvm_greatest)"]=my-pkg-install
    [corrector_of_llvm_xray_stack_flame]=my-cargo-install-from-my-repo
                             [emacs-nox]=my-pkg-install
                             [diffutils]=my-pkg-install
                               [fd-find]=my-pkg-install
                                   [gcc]=my-pkg-install
                                   [gdb]=my-pkg-install
                                   [git]=my-pkg-install
                                 [groff]=my-pkg-install
                                 [gmake]=my-pkg-install
                               [gnugrep]=my-pkg-install
                                 [gnupg]=my-pkg-install
                                  [gsed]=my-pkg-install
                                  [gtar]=my-pkg-install
                                  [htop]=my-pkg-install
                                  [lsof]=my-pkg-install
                                  [most]=my-pkg-install
              [my_bash_history_combiner]=my-cargo-install-from-my-repo
                     [my-emacs-packages]=my-install-emacs-packages
                                  [nano]=my-pkg-install
                                   [npm]=my-pkg-install
                                 [patch]=my-pkg-install
                          [pkg-provides]=my-pkg-install
                                [psmisc]=my-pkg-install
                               [ripgrep]=my-pkg-install
                                  [rust]=my-pkg-install
                         [rust-analyzer]=my-pkg-install
                                [screen]=my-pkg-install
                         [hs-ShellCheck]=my-pkg-install
                           [terminfo-db]=my-pkg-install
                                         # `flock` is needed by _my_lock_combined_histfile.  Might
                                         # as well enable most of the other "options" since we're
                                         # building it anyway, and I'm more familiar with using
                                         # these Linux utilities than FreeBSD's analogues.  But
                                         # don't enable the "UUID" option because it attempts to
                                         # install /usr/local/bin/uuidgen which conflicts with
                                         # e2fsprogs-libuuid having already installed that file.
                                         # Some of the executable names are the same as some of
                                         # FreeBSD's standard utilities, but FreeBSD's default
                                         # PATH places /usr/local/bin with lower precedence and so
                                         # its standard utilities will still be the ones used by
                                         # default.  I could arrange some other way(s) to use the
                                         # util-linux ones by default, if I want.
                      [devel/util-linux]='single my-port-install WITH="CAL FLOCK GETOPT HARDLINK"'
                            [wrk-luajit]=my-pkg-install
)

unset -f _my_llvm_greatest


function my-pkg-install {
    sudo pkg install --yes "$@"
}

function is-pkg-installed {
    pkg info "$@" > /dev/null 2>&1
}


# Install GNU `coreutils` immediately, because it's used for installing other packages.
if ! is-pkg-installed coreutils ; then
    my-pkg-install coreutils || warn "Couldn't install GNU coreutils. Hopefully it already is."
fi


function _my_freebsd_quarterly {
    local - ; set -o nounset
    local -r when=${1:-now}
    local yearAndMonth splitted year month quarter

    yearAndMonth=$(gnu date --date="$when" "+%Y %m") || return
    split-on-words "$yearAndMonth" splitted || return
    (( ${#splitted[@]} == 2 )) || return
    year=${splitted[0]}
    month=${splitted[1]}
    month=$(shopt -s extglob; print "${month##+(0)}")  # Don't try to interpret as octal.

    if (( month <= 3 )); then
        quarter=1
    elif (( month <= 6 )); then
        quarter=2
    elif (( month <= 9 )); then
        quarter=3
    else
        quarter=4
    fi
    print "${year}Q$quarter"
}

function _my_freebsd_clone_ports_collection {
    local - ; set -o nounset
    local -r url="https://git.FreeBSD.org/ports.git"
    local dest=() branch=() super=()

    if (( $# >= 1 )); then
        dest=("$1")
    fi
    if (( $# >= 2 )); then
        branch=(--branch "$2")
    fi
    if ! [ -w "$(std dirname "${1:-.}")" ]; then
        super=(sudo)
    fi
    "${super[@]}" git clone --depth 1 "${branch[@]}" "$url" "${dest[@]}"
}

function _my_freebsd_install_ports_collection {
    local - ; set -o nounset

    if ( shopt -s nullglob dotglob
         local -r portsEntries=(/usr/ports/*)
         (( ${#portsEntries[@]} == 0 )) )
    then
        if ! is-command-found git ; then
            my-platform-install-packages git || return  # Ensure it's installed.
        fi

        # Using the latest "Quarterly" branch releases, when `pkg` does also (which is the usual
        # default), is recommended by the FreeBSD Handbook's "Using the Ports Collection" section
        # (https://docs.freebsd.org/en/books/handbook/ports/#ports-using)
        local quarterly ; quarterly=$(_my_freebsd_quarterly) || return

        if ! _my_freebsd_clone_ports_collection /usr/ports "$quarterly"
        then
            warn "Quarterly ${quarterly@Q} not found as a branch. Trying previous."
            # Assume they haven't published the next Quarterly quite yet, and assume the most
            # recent must be one quarter earlier and assume that was at most a month ago.
            quarterly=$(_my_freebsd_quarterly "1 month ago") || return

            if ! _my_freebsd_clone_ports_collection /usr/ports "$quarterly"
            then
                warn "Quarterly ${quarterly@Q} not found as a branch. Trying without."
                _my_freebsd_clone_ports_collection /usr/ports
            fi
        fi
    fi
}

function my-port-install {
    local - ; set -o nounset
    local -r args=("$@")
    local -r portName="${args[-1]}" opts=("${args[@]:0:${#args[@]}-1}")

    is-pkg-installed "$portName" && return

    _my_freebsd_install_ports_collection || return  # Ensure the Ports Collection is present.

    if ! [ -d /usr/ports/"$portName" ]; then
        error "Port name ${portName@Q} is not present in /usr/ports/!"
        return 2
    fi

    ( cd /usr/ports/"$portName" || exit

      local -r envVars=(
         #PREFIX=  # Where to install this port.  Defaults to /usr/local/.
          BATCH=yes  # Only operate on a port if it can be installed 100% automatically.
          ASSUME_ALWAYS_YES=yes  # For the `pkg` operations done by `install-missing-packages`.
          WRKDIRPREFIX="${TMPDIR:-/tmp}"/my-ports/working-dir  # Where to create temp files.
      )
      local -r targets=(
          install-missing-packages  # Install dependencies via `pkg` instead of building them.
          install  # Install the port and register it with the package system.
          clean  # Remove the expanded source code. This recurses to dependencies.
      )

      sudo "${envVars[@]}" make "${opts[@]}" "${targets[@]}"
    )
}
