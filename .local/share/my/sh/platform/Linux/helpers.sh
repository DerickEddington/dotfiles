# shellcheck disable=SC2034  # These variables are used by what `source`s this file.


# Avoid source'ing this file more than once.
#
[ "${_MY_SH_SOURCED_ALREADY__PLATFORM_OS_HELPERS+is-set}" ] && return
# Set immediately, to avoid the possibility of recursive source'ing.
_MY_SH_SOURCED_ALREADY__PLATFORM_OS_HELPERS=true


# Platform-specific identification

if [ "${MY_OS_RELEASE_FILE-}" ]
then
    _my_set_id_and_version_from_os_release_file MY_PLATFORM_VARIANT MY_PLATFORM_VERSION

elif is_command_found lsb_release
then
    MY_PLATFORM_VARIANT=$(lsb_release --id --short)
    MY_PLATFORM_VERSION=$(lsb_release --release --short)
    if [ "$MY_PLATFORM_VERSION" = "n/a" ]; then
        MY_PLATFORM_VERSION=$(lsb_release --codename --short)
    fi

    # Must remove any quotes that some platforms output.
    # E.g.:
    #   $ println "$(lsb_release -s -i)/$(lsb_release -s -r)"
    #   "NixOS"/"23.05"
    # Versus:
    #   $ println "$(lsb_release -s -i)/$(lsb_release -s -r)"
    #   Ubuntu/22.04
    #
    MY_PLATFORM_VARIANT=$(remove_surrounding_quotes "$MY_PLATFORM_VARIANT")
    MY_PLATFORM_VERSION=$(remove_surrounding_quotes "$MY_PLATFORM_VERSION")
fi

readonly MY_PLATFORM_VARIANT  # Not MY_PLATFORM_VERSION yet, to allow sub-helpers.sh to change it.

if ! [ "${MY_PLATFORM_VARIANT-}" ] && ! [ "${MY_PLATFORM_VERSION-}" ]
then
    warn "Don't know how to further identify this $MY_PLATFORM_OS platform!"
fi


# Functions

gnu() {
    std "$@"  # Assume that GNU utilities are the default.
}

_my_terminal_supports_colors()
{
    if is_command_found tput -p ; then
        std tput setaf 1 > /dev/null 2>&1
    else
        case "${TERM:-}" in
            (*color*) return 0 ;;
            (*)       return 1 ;;
        esac
    fi
}

_my_terminal_width() {
    std tput cols
}

_my_flock() {
    std flock "$@"
}
