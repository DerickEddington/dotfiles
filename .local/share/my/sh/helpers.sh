# General helpers for both scripted and interactive shell use.
# This must be compliant with POSIX `sh` and POSIX utilities.
# Source'ing this must be idempotent, in case it's ever source'd multiple times.
# This must not source files via relative paths, because `sh` isn't good for that.

# shellcheck disable=SC2034  # These variables are used by the things that source this.


# Avoid source'ing this file more than once.
#
[ "${_MY_SH_SOURCED_ALREADY__HELPERS+is-set}" ] && return
# Set immediately, to avoid the possibility of recursive source'ing.
_MY_SH_SOURCED_ALREADY__HELPERS=true


warn() {
    echo "Warning${1:+: $1}" 1>&2
    return "${2:-0}"
}


# XDG Base Directory Specification
# https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
#
MY_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
MY_DATA_HOME=${XDG_DATA_HOME:-$HOME/.local/share}
MY_STATE_HOME=${XDG_STATE_HOME:-$HOME/.local/state}
MY_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
if [ "${XDG_RUNTIME_DIR:-}" ]; then
    MY_RUNTIME_DIR=$XDG_RUNTIME_DIR
else
    MY_RUNTIME_DIR=${TMPDIR:-/tmp}/user/${USER:-$(id -u)}
    warn "XDG_RUNTIME_DIR is undefined. Will try to use $MY_RUNTIME_DIR/."
    mkdir -m a=rwXt "$(dirname "$MY_RUNTIME_DIR")" > /dev/null 2>&1 || true
    mkdir -m u=rwX,g=,o= "$MY_RUNTIME_DIR"
fi


# Functions

error() {
    echo "Error${1:+: $1}" 1>&2
    return "${2:-0}"
}

fail() {
    error "${1:-}" || true
    exit "${2:-1}"
}

assert_nonexistent() {
    [ ! -e "${1:-}" ] || fail "$1 already exists!"
}

assert_all_nonexistent() {
    while [ $# -ge 1 ]; do
        assert_nonexistent "$1"
        shift
    done
}

std() {
    command -p -- "$@"  # TODO: Is the `--` portable enough?
}


# Any source'ing of sub files must be done below here, so that the above are all defined for such.


# Platform-specific identification
#
MY_PLATFORM=$(uname)/$(uname -r)           # This is often changed by next `source` to be better.
MY_PLATFORM_ARCH=$MY_PLATFORM/$(uname -m)  # Ditto.
if [ -e "$MY_DATA_HOME"/my/platform/"$(uname)"/helpers.sh ]; then
    # shellcheck source=../platform/Linux/helpers.sh  #  (Just one of many, to have something.)
    . "$MY_DATA_HOME"/my/platform/"$(uname)"/helpers.sh
fi
readonly MY_PLATFORM MY_PLATFORM_ARCH
