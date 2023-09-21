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


# Functions used immediately below.

warn() {
    echo "Warning${1:+: $1}" 1>&2
    return "${2:-0}"
}

error() {
    echo "Error${1:+: $1}" 1>&2
    return "${2:-0}"
}

fail() {
    error "${1:-}" || true
    exit "${2:-1}"
}

assert_nonnull() {
    while [ $# -ge 1 ]; do
        eval "[ \"\${${1}:-}\" ]" || fail "Parameter '$1' is null or unset!"
        shift
    done
}

std() {
    command -p -- "$@"  # TODO: Is the `--` portable enough?
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
    # shellcheck disable=SC2174
    mkdir -p -m a=rwXt "$(std dirname "$MY_RUNTIME_DIR")"
    # shellcheck disable=SC2174
    mkdir -p -m u=rwX,g=,o= "$MY_RUNTIME_DIR"
fi
readonly MY_CONFIG_HOME MY_DATA_HOME MY_STATE_HOME MY_CACHE_HOME MY_RUNTIME_DIR
assert_nonnull MY_CONFIG_HOME MY_DATA_HOME MY_STATE_HOME MY_CACHE_HOME MY_RUNTIME_DIR


# Functions

is_command_found() {
    [ "${2--p}" = '-p' ] || exit

    if command ${2:-} -v "${1:-}" > /dev/null 1>&2 ; then
        return 0
    else
        if [ "${2:-}" = '-p' ]
        then
            command -v "${1:-}" > /dev/null 1>&2 \
                && warn "Command '${1:-}' found in current PATH but not default PATH."
        else
            command -p -v "${1:-}" > /dev/null 1>&2 \
                && warn "Command '${1:-}' found in default PATH but not current PATH."
        fi
        return 1
    fi
}

# POSIX-Shell-quoted form of arbitrary string (http://www.etalabs.net/sh_tricks.html).
# (Note: Transformations like Bash's `${var@Q}` or `printf %q` are not suitable for
# POSIX-Shell-conformance portability, because those can produce forms like `$'...\n...'` which
# are not valid POSIX-Shell syntax.)
quote() {
    std printf '%s' "${1:-}" | std sed "s/'/'\\\\''/g;1s/^/'/;\$s/\$/'/"
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

_my_install_critical_util_if_needed() {
    if ! is_command_found "${1:?}" ; then
        if is_command_found _my_install_"$1" ; then
            _my_install_"$1" || fail "Failed to install ${1:-}!" 64
        else
            fail "Missing _my_install_$1 for platform ${MY_PLATFORM_OS_VARIANT}!" 65
        fi
    fi
}

_my_install_bash_if_needed()            { _my_install_critical_util_if_needed bash ;}
_my_install_git_if_needed()             { _my_install_critical_util_if_needed git ;}


# Any source'ing of sub files must be done below here, so that the above are all defined for such.


# Platform-specific identification

MY_PLATFORM_OS=$(std uname)       # 1 component. E.g.: Linux, FreeBSD, SunOS, etc.
MY_PLATFORM_ARCH=$(std uname -m)  # 1 component. E.g.: x86_64, amd64, i86pc, etc.
readonly MY_PLATFORM_OS MY_PLATFORM_ARCH
assert_nonnull MY_PLATFORM_OS MY_PLATFORM_ARCH

# These must be defined by the next `source`:
#   MY_PLATFORM_VARIANT  # 0 or 1 component. E.g.: Ubuntu, OpenIndiana, or empty for FreeBSD.
#   MY_PLATFORM_VERSION  # 1 component. E.g.: 22.04, 13, etc.

# These will be automatically defined:
#   MY_PLATFORM_OS_VARIANT  # 1 or 2 component. E.g.: Linux/Ubuntu, SunOS/OpenIndiana, or FreeBSD.
#   MY_PLATFORM_OS_VAR_VER  # 2 or 3 component. E.g.: Linux/Ubuntu/22.04, or FreeBSD/13.

if [ -e "$MY_DATA_HOME"/my/sh/platform/"$MY_PLATFORM_OS"/helpers.sh ]; then
    # shellcheck source=./platform/Linux/helpers.sh  # (Just one of many, to have something.)
    . "$MY_DATA_HOME"/my/sh/platform/"$MY_PLATFORM_OS"/helpers.sh
fi
readonly MY_PLATFORM_VARIANT MY_PLATFORM_VERSION
assert_nonnull MY_PLATFORM_VERSION

readonly MY_PLATFORM_OS_VARIANT="$MY_PLATFORM_OS${MY_PLATFORM_VARIANT:+/$MY_PLATFORM_VARIANT}"
readonly MY_PLATFORM_OS_VAR_VER="$MY_PLATFORM_OS_VARIANT/$MY_PLATFORM_VERSION"
assert_nonnull MY_PLATFORM_OS_VARIANT MY_PLATFORM_OS_VAR_VER

if [ "${MY_PLATFORM_VARIANT:-}" ]; then
    if [ -e "$MY_DATA_HOME"/my/sh/platform/"$MY_PLATFORM_OS_VARIANT"/helpers.sh ]; then
        # shellcheck source=/dev/null  # (Don't care if there isn't one.)
        . "$MY_DATA_HOME"/my/sh/platform/"$MY_PLATFORM_OS_VARIANT"/helpers.sh
    fi
fi

if [ -e "$MY_DATA_HOME"/my/sh/platform/"$MY_PLATFORM_OS_VAR_VER"/helpers.sh ]; then
    # shellcheck source=/dev/null  # (Don't care if there isn't one.)
    . "$MY_DATA_HOME"/my/sh/platform/"$MY_PLATFORM_OS_VAR_VER"/helpers.sh
fi
