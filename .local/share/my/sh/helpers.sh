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


# Functions

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

is_stdin_a_tty() {
    std tty > /dev/null
}

# POSIX-Shell-quoted form of arbitrary string (http://www.etalabs.net/sh_tricks.html).
# (Note: Transformations like Bash's `${var@Q}` or `printf %q` are not suitable for
# POSIX-Shell-conformance portability, because those can produce forms like `$'...\n...'` which
# are not valid POSIX-Shell syntax.)
quote() {
    std printf '%s' "${1:-}" | std sed "s/'/'\\\\''/g;1s/^/'/;\$s/\$/'/"
}

remove_surrounding_quotes() { (
    it=${1?}
    case "$it" in
        (\"*) it=${it#\"} ; it=${it%\"} ;;
        (\'*) it=${it#\'} ; it=${it%\'} ;;
    esac
    print "$it"
) }

lowercase() {
    print "${1?}" | std tr '[:upper:]' '[:lower:]'
}

capitalize() { (
    it=${1?}
    # Must give "text" (lines w/ newlines) to `cut`.  Use command substitutions to remove the
    # trailing newlines.
    C=$(println "$it" | std cut -c1 | std tr '[:lower:]' '[:upper:]')
    it=${C}$(println "$it" | std cut -c2-)
    print "$it"
) }

abs_path() { ( set -u -e
    case "${1:?}" in
        (/*)
            print "$1" ;;
        (*)
            absCurDir=$(std pwd -L)  # Keeps symlinks (if possible)
            print "${absCurDir%/}"/"$1"
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

is_in_colon_list() {
    println "${2-}" | std grep -q -E -e "(^|:)${1-}(:|\$)"
}

prepend_to_colon_list() {
    print "${1-}${2:+:$2}"
}

prepend_to_colon_list_var_export() {
    eval "$(
        element=${1?}
        varName=${2:?}
        eval "varValue=\${${varName}-}"
        element=$(quote "$element")
        varValue=$(quote "$varValue")
        print "${varName}=\$(prepend_to_colon_list $element $varValue)"
    )" || return
    eval "export ${2:?}"
}

prepend_to_colon_list_var_export_if_ok() {
    if [ -d "${1:?}" ]; then
        if ! eval "is_in_colon_list \"\$1\" \"\${${2:?}-}\"" ; then
            prepend_to_colon_list_var_export "$1" "$2"
        fi
    fi
}

prepend_to_PATH_if_ok() {
    prepend_to_colon_list_var_export_if_ok "${1:?}" PATH
}

prepend_to_LD_LIBRARY_PATH_if_ok() {
    # This doesn't support "A zero-length directory name indicates the current working directory",
    # because that's Linux-specific.  Instead, give `.` which seems more portable (TODO: this
    # point is untested).
    prepend_to_colon_list_var_export_if_ok "${1:?}" LD_LIBRARY_PATH
}

prepend_and_subs_to_colon_list_var_export_if_ok() {
    prepend_to_colon_list_var_export_if_ok "${1:?}" "${2:?}"
    # Include sub-dirs also.  This can be especially convenient for symlink'ing like:
    # ~/bin/thing-0.42 -> ~/tmp/thing-0.42/bin.
    for subDir in "$1"/* ; do
        if [ "$(std basename "$subDir")" != "my" ]; then  # Exclude the special `my` sub-dir.
            prepend_to_colon_list_var_export_if_ok "$subDir" "$2"
        fi
    done
    unset subDir
}

prepend_and_subs_to_PATH_if_ok() {
    prepend_and_subs_to_colon_list_var_export_if_ok "${1:?}" PATH
}

prepend_and_subs_to_LD_LIBRARY_PATH_if_ok() {
    prepend_and_subs_to_colon_list_var_export_if_ok "${1:?}" LD_LIBRARY_PATH
}

_my_script_prelude() {
    set -e -u  # -o errexit -o nounset

    readonly self="$0"  # The same $0 as outside a function.
    selfBase=$(std basename "$self")
    selfDir=$(std dirname "$self")
    selfDirAbs=$(abs_path "$selfDir")
    selfDirNorm=$(norm_abs_path "$selfDirAbs") || true
    readonly selfBase selfDir selfDirAbs selfDirNorm

    # Prevent my scripts from using `echo`.  (http://www.etalabs.net/sh_tricks.html)
    # shellcheck disable=SC2317
    echo() {
        # shellcheck disable=SC2016
        error 'Don'\''t use `echo`! It'\''s unportable and unreliable! Use my `println` (etc).'
        if is_shell_interactive; then return 42; else exit 42; fi
    }

    [ "${VERBOSE:=0}" -ge 5 ] && set -x
    [ "${VERBOSE:=0}" -ge 6 ] && set -v

    true
}

_my_install_critical_util_if_needed() {
    if ! is_command_found "${1:?}" ; then
        if is_command_found _my_install_"$1" ; then
            _my_install_"$1" || fail "Failed to install ${1:-}!" 64
            if ! is_command_found "$1" ; then
                fail "Still missing $1 after install!" 66
            fi
        else
            fail "Missing _my_install_$1 for platform ${MY_PLATFORM_OS_VARIANT:-unknown}!" 65
        fi
    fi
}

_my_install_bash_if_needed()            { _my_install_critical_util_if_needed bash ;}
_my_install_git_if_needed()             { _my_install_critical_util_if_needed git ;}

# shellcheck disable=SC2174
_my_make_runtime_dir_in_tmp() { ( set -u -e  # Return error code if any command errors.
    { tmpUsersDir=${TMPDIR:-/tmp}/user
      dir=$tmpUsersDir/$(userName_canon)
      std mkdir -p -m a=rwXt "$tmpUsersDir"
      std mkdir -p -m u=rwX,g=,o= "$dir"
    } > /dev/null
    print "$dir"
) }

_my_sh_helpers__set_XDG_BDS()
{
    # XDG Base Directory Specification
    # https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html

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
}

_my_sh_helpers__set_platform_identification()
{
    # Platform-specific identification

    MY_PLATFORM_OS=$(std uname)       # 1 component. E.g.: Linux, FreeBSD, SunOS, etc.
    MY_PLATFORM_ARCH=$(std uname -m)  # 1 component. E.g.: x86_64, amd64, i86pc, etc.
    MY_PLATFORM_OS_ARCH=$MY_PLATFORM_OS/$MY_PLATFORM_ARCH
    readonly MY_PLATFORM_OS MY_PLATFORM_ARCH MY_PLATFORM_OS_ARCH
    assert_nonnull MY_PLATFORM_OS MY_PLATFORM_ARCH MY_PLATFORM_OS_ARCH

    [ -r /usr/lib/os-release ] && MY_OS_RELEASE_FILE=/usr/lib/os-release
    [ -r /etc/os-release ]     && MY_OS_RELEASE_FILE=/etc/os-release
    readonly MY_OS_RELEASE_FILE

    # These must be defined by the next `source`:
    #   MY_PLATFORM_VARIANT  # 0 or 1 component. E.g.: Ubuntu, OpenIndiana, or empty for FreeBSD.
    #   MY_PLATFORM_VERSION  # 0 or 1 component. E.g.: 22.04, 13, trixie, or empty for Arch Linux.

    # These will be automatically defined:
    #   MY_PLATFORM_OS_VARIANT       # 1,2. E.g.: Linux/Ubuntu, SunOS/OpenIndiana, or FreeBSD.
    #   MY_PLATFORM_OS_VARIANT_ARCH  # 2,3. E.g.: Linux/Ubuntu/x86_64, or FreeBSD/amd64.
    #   MY_PLATFORM_OS_VAR_VER       # 1,3. E.g.: Linux/Ubuntu/22.04, FreeBSD/13, Linux/Arch.
    #   MY_PLATFORM_OS_VAR_VER_ARCH  # 2,4. E.g.: Linux/Ubuntu/22.04/x86_64, or FreeBSD/13/amd64.

    if [ -e "$MY_DATA_HOME"/my/sh/platform/"$MY_PLATFORM_OS"/helpers.sh ]; then
        # shellcheck source=./platform/Linux/helpers.sh  # (Just one of many, to have something.)
        . "$MY_DATA_HOME"/my/sh/platform/"$MY_PLATFORM_OS"/helpers.sh
    fi
    readonly MY_PLATFORM_VARIANT MY_PLATFORM_VERSION

    MY_PLATFORM_OS_VARIANT="$MY_PLATFORM_OS${MY_PLATFORM_VARIANT:+/$MY_PLATFORM_VARIANT}"
    MY_PLATFORM_OS_VAR_VER="$MY_PLATFORM_OS_VARIANT${MY_PLATFORM_VERSION:+/$MY_PLATFORM_VERSION}"
    readonly MY_PLATFORM_OS_VARIANT MY_PLATFORM_OS_VAR_VER
    assert_nonnull MY_PLATFORM_OS_VARIANT MY_PLATFORM_OS_VAR_VER

    readonly MY_PLATFORM_OS_VARIANT_ARCH="$MY_PLATFORM_OS_VARIANT"/"$MY_PLATFORM_ARCH"
    readonly MY_PLATFORM_OS_VAR_VER_ARCH="$MY_PLATFORM_OS_VAR_VER"/"$MY_PLATFORM_ARCH"
    assert_nonnull MY_PLATFORM_OS_VARIANT_ARCH MY_PLATFORM_OS_VAR_VER_ARCH

    if [ "${MY_PLATFORM_VARIANT:-}" ]; then
        if [ -e "$MY_DATA_HOME"/my/sh/platform/"$MY_PLATFORM_OS_VARIANT"/helpers.sh ]; then
            # shellcheck source=/dev/null  # (Don't care if there isn't one.)
            . "$MY_DATA_HOME"/my/sh/platform/"$MY_PLATFORM_OS_VARIANT"/helpers.sh
        fi
    fi

    if [ "${MY_PLATFORM_VERSION:-}" ]; then
        if [ -e "$MY_DATA_HOME"/my/sh/platform/"$MY_PLATFORM_OS_VAR_VER"/helpers.sh ]; then
            # shellcheck source=/dev/null  # (Don't care if there isn't one.)
            . "$MY_DATA_HOME"/my/sh/platform/"$MY_PLATFORM_OS_VAR_VER"/helpers.sh
        fi
    fi
}

_my_sh_helpers__finish() {
    if ! [ "${_MY_SH_HELPERS__IS_FINISHED+is-set}" ]; then
        _my_sh_helpers__set_XDG_BDS
        _my_sh_helpers__set_platform_identification
        _MY_SH_HELPERS__IS_FINISHED=true
    fi
}

_my_set_id_and_version_from_os_release_file()
{
    # This is a reusable helper, because OSs other than Linux+systemd, e.g. FreeBSD, sometimes
    # also support the `os-release` file.  Note that for some, e.g. FreeBSD, the $ID is the same
    # as $MY_PLATFORM_OS and so uses of this function for such must adjust for this (e.g. by
    # ignoring MY_OS_RELEASE_ID, not using it for MY_PLATFORM_VARIANT, but using
    # MY_OS_RELEASE_VERSION for MY_PLATFORM_VERSION).

    if [ "${MY_OS_RELEASE_FILE-}" ]
    then
        _MY_OS_RELEASE_ID=$(
            unset ID NAME

            # shellcheck source=/etc/os-release
            . "$MY_OS_RELEASE_FILE" > /dev/null

            if [ "${NAME-}" ]; then
                NAME=${NAME%% *}  # Keep only the first word.
            fi
            if [ "${ID-}" ]; then
                # If NAME is set, its first word, if the same as $ID case-insensitively, probably
                # has better casing than ID capitalized (e.g. "NixOS" versus "Nixos").
                if [ "$(lowercase "${NAME-}")" = "$(lowercase "$ID")" ]; then
                    ID=$NAME
                fi
            elif [ "${NAME-}" ]; then
                ID=$NAME
            fi

            if [ "${ID-}" ]; then
                print "$(capitalize "$ID")"
            fi
        )

        _MY_OS_RELEASE_VERSION=$(
            unset VERSION_ID VERSION_CODENAME

            # shellcheck source=/etc/os-release
            . "$MY_OS_RELEASE_FILE" > /dev/null

            if [ "${VERSION_ID-}" ]; then
                print "$VERSION_ID"
            elif [ "${VERSION_CODENAME-}" ]; then
                print "$VERSION_CODENAME"
            fi
        )

        # The caller chooses which variables to assign these to.
        #
        eval "${1:?}=$(quote "$_MY_OS_RELEASE_ID")"
        eval "${2:?}=$(quote "$_MY_OS_RELEASE_VERSION")"

        unset _MY_OS_RELEASE_ID _MY_OS_RELEASE_VERSION
    else
        return 1
    fi
}


# Any source'ing of sub files must be done below here, so that the above are all defined for such.


# Run when source'd.
#
if ! [ "${_MY_SH_HELPERS__ONLY_FUNCTIONS+is-set}" ]; then
    _my_sh_helpers__finish
fi
