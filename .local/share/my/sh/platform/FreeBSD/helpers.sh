# shellcheck disable=SC2034  # These variables are used by what `source`s this file.


# Avoid source'ing this file more than once.
#
[ "${_MY_SH_SOURCED_ALREADY__PLATFORM_HELPERS+is-set}" ] && return
# Set immediately, to avoid the possibility of recursive source'ing.
_MY_SH_SOURCED_ALREADY__PLATFORM_HELPERS=true


# Platform-specific identification

MY_PLATFORM=$(uname -r)
MY_PLATFORM=${MY_PLATFORM%%.*}  # TODO: Proper for FreeBSD's release ID format?
MY_PLATFORM=$(uname)/$MY_PLATFORM

MY_PLATFORM_ARCH=$MY_PLATFORM/$(uname -m)

readonly MY_PLATFORM MY_PLATFORM_ARCH


# Functions

gnu() {
    [ $# -ge 1 ] || return 1

    # shellcheck disable=SC2145  # I know how `some"$@"` behaves and it's correct.

    if [ -x /usr/local/bin/gnu"$1" ]; then
        /usr/local/bin/gnu"$@"
    elif [ -x /usr/local/bin/g"$1" ]; then
        /usr/local/bin/g"$@"
    else
        error "No 'gnu' nor 'g' prefixed utility found for '$1'!"
        return 2
    fi
}

_my_terminal_supports_colors() {
    case "${TERM:-}" in
        (*color*) return 0 ;;
        (*)       return 1 ;;
    esac
}

_my_flock() {
    /usr/local/bin/flock "$@"  # The `flock` of util-linux.
}
