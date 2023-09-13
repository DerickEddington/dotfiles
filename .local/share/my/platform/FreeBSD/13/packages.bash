# Defines the mapping of my package names to their platform-specific packages and how to install
# those.

# shellcheck disable=SC2034  # These variables are used by what `source`s this file.


source "$(dirname "${BASH_SOURCE[0]}")"/../../../bash/helpers.bash


# If already source'd, don't do anything.
_my_bash_sourced_already local/share/my/platform/FreeBSD/13/packages && return


# Maps my own convention of a package name to its platform-specific package name.
#
readonly -A MY_PLATFORM_SPECIFIC_PACKAGES_NAMES=(
    [bash-completion]=bash-completion
             [clangd]=llvm16
            [fd-find]=fd-find
                [git]=git
      [gnu-coreutils]=coreutils
               [htop]=htop
               [most]=most
               [nano]=nano
            [ripgrep]=ripgrep
             [screen]=screen
         [util-linux]=devel/util-linux
    # TODO: the others
)

# Maps a platform-specific package name to its platform-specific command for installing it.  Each
# value (a command) may be multiple words quoted (e.g. to pass options to a command).
#
readonly -A MY_PLATFORM_SPECIFIC_PACKAGES_METHODS=(
     [bash-completion]=my-pkg-install
           [coreutils]=my-pkg-install
              [llvm16]=my-pkg-install
             [fd-find]=my-pkg-install
                 [git]=my-pkg-install
                [htop]=my-pkg-install
                [most]=my-pkg-install
                [nano]=my-pkg-install
             [ripgrep]=my-pkg-install
              [screen]=my-pkg-install
                       # `flock` is needed by _my_lock_combined_histfile.  Might as well enable
                       # all the other "options" since we're building it anyway and I'm more
                       # familiar with these Linux utilities.  Some of their executable names are
                       # the same as some of FreeBSD's standard utilities, but FreeBSD's default
                       # PATH places /usr/local/bin with lower precedence and so its standard
                       # utilities will still be the ones used by default.  I could arrange some
                       # other way(s) to use the util-linux ones by default, if I want.
    [devel/util-linux]='my-single-port-install WITH="CAL FLOCK GETOPT HARDLINK UUID"'
    # TODO: the others
)


function my-pkg-install {
    sudo pkg install --yes "$@"
}


function _my_freebsd_quarterly {
    local - ; set -o nounset
    local -r when=${1:-now}
    local yearAndMonth splitted year month quarter

    yearAndMonth=$(gnu date --date="$when" "+%Y %m") || return
    split-on-words "$yearAndMonth" splitted || return
    (( ${#splitted[@]} == 2 )) || return
    year=${splitted[0]}
    month=${splitted[1]}
    month=$(shopt -s extglob; echo "${month##+(0)}")  # Don't try to interpret as octal.

    if (( month <= 3 )); then
        quarter=1
    elif (( month <= 6 )); then
        quarter=2
    elif (( month <= 9 )); then
        quarter=3
    else
        quarter=4
    fi
    echo "${year}Q$quarter"
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

    if ! [ -d /usr/ports ]
    then
        if ! command -v git > /dev/null 1>&2 ; then
            my-pkg-install git || return  # Ensure it's installed.
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

function my-single-port-install {
    local - ; set -o nounset
    local -r args=("$@")
    local -r portName="${args[-1]}" opts=("${args[@]:0:${#args[@]}-1}")

    _my_freebsd_install_ports_collection || return  # Ensure the Ports Collection is present.

    if ! [ -d /usr/ports/"$portName" ]; then
        error "Port name ${portName@Q} is not present in /usr/ports/!"
        return 2
    fi

    ( cd /usr/ports"$portName"

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
