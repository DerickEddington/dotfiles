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

std() {
    command -p -- "$@"
}

print()    { std printf '%s'   "$*" ;}
println()  { std printf '%s\n' "$*" ;}
eprint()   { print   "$@" 1>&2 ;}
eprintln() { println "$@" 1>&2 ;}

warn() {
    eprintln "Warning${1:+: $1}"
    return "${2:-0}"
}

error() {
    eprintln "Error${1:+: $1}"
    return "${2:-0}"
}

fail() {
    error "${1:-}" || true
    exit "${2:-1}"
}

assert_nonnull() {
    while [ $# -ge 1 ]; do
        eval "[ \"\${${1}:-}\" ]" || fail "Parameter $(quote "$1") is null or unset!"
        shift
    done
}

userName_canon() { ( set -e -u
    std id -u > /dev/null || exit  # Ensure this is working at least.
    u=$(std logname || std id -u -n || std id -u)  # Return error code (via `set -e`) if error.
    print "$u"
) }

userName_given() { ( set -e -u
    if [ "${USER:-}" ]; then
        print "$USER"
    else
        u=$(userName_canon)  # Return error code (via `set -e`) if error.
        print "$u"
    fi
) }

# shellcheck disable=SC2174
_my_make_runtime_dir_in_tmp() { ( set -u -e  # Return error code if any command errors.
    { tmpUsersDir=${TMPDIR:-/tmp}/user
      dir=$tmpUsersDir/$(userName_canon)
      std mkdir -p -m a=rwXt "$tmpUsersDir"
      std mkdir -p -m u=rwX,g=,o= "$dir"
    } > /dev/null
    print "$dir"
) }


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
    MY_RUNTIME_DIR=$(_my_make_runtime_dir_in_tmp) || return
    warn "XDG_RUNTIME_DIR is undefined. Will try to use $MY_RUNTIME_DIR/."
fi
readonly MY_CONFIG_HOME MY_DATA_HOME MY_STATE_HOME MY_CACHE_HOME MY_RUNTIME_DIR
assert_nonnull MY_CONFIG_HOME MY_DATA_HOME MY_STATE_HOME MY_CACHE_HOME MY_RUNTIME_DIR


# Functions

is_command_found() {
    [ "${2--p}" = '-p' ] || exit

    if command ${2:-} -v "${1:-}" > /dev/null 2>&1 ; then
        return 0
    else
        if [ "${2:-}" = '-p' ]
        then
            command -v "${1:-}" > /dev/null 2>&1 \
                && warn "Command $(quote "${1:-}") found in current PATH but not default PATH."
        else
            command -p -v "${1:-}" > /dev/null 2>&1 \
                && warn "Command $(quote "${1:-}") found in default PATH but not current PATH."
        fi
        return 1
    fi
}

is_shell_interactive() {
    case "$-" in
        (*i*) return 0 ;;
        (*)   return 1 ;;
    esac
}

if is_shell_interactive ; then
    :  # Can't change it.  Doing so would break many tab-completion functions' use of it.
else
    # Prevent my scripts from using it.  (http://www.etalabs.net/sh_tricks.html)
    echo() {
        # shellcheck disable=SC2016
        error 'Don'\''t use `echo`! It'\''s unportable and unreliable! Use my `println` (etc).'
        if is_shell_interactive; then return 42; else exit 42; fi
    }
fi

# POSIX-Shell-quoted form of arbitrary string (http://www.etalabs.net/sh_tricks.html).
# (Note: Transformations like Bash's `${var@Q}` or `printf %q` are not suitable for
# POSIX-Shell-conformance portability, because those can produce forms like `$'...\n...'` which
# are not valid POSIX-Shell syntax.)
quote() {
    std printf '%s' "${1:-}" | std sed "s/'/'\\\\''/g;1s/^/'/;\$s/\$/'/"
}

abs_path() { ( set -u -e
    case "${1:?}" in
        (/*)
            print "$1" ;;
        (*)
            absCurDir=$(std pwd -L)
            if [ "$absCurDir" = '/' ]; then absCurDir=''; fi
            print "$absCurDir"/"$1"  # Keeps symlinks (if possible)
            ;;
    esac
) }

norm_abs_path() { ( set -u -e
    n=$(abs_path "$1")  # Also avoids `cd`'s special interpretation of '-' if $1 is '-'.
    unset CDPATH  # Avoid `cd`'s use of this.

    if [ -d "$n" ]; then
        # To normalize '.' & '..' components, do `cd`.
        # To be consistent with `[ -d` and with normal pathname resolution, do `cd -P`.
        # To normalize to have symlink components resolved, do `pwd -P`.
        n=$(std cd -P -- "$n" && std pwd -P)
    else
        dir=$(std dirname "$n")
        base=$(std basename "$n")
        if [ -d "$dir" ]; then
            n=$(std cd -P -- "$dir" && std pwd -P)
            if [ "$n" = '/' ]; then n=''; fi
            n=$n/$base
        else
            exit 1  # It can't exist with non-existent parent.
        fi
    fi

    print "$n"
) }

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
