# shellcheck disable=SC2034  # These variables are used by what `source`s this file.


# Avoid source'ing this file more than once.
#
[ "${_MY_SH_SOURCED_ALREADY__PLATFORM_OS_HELPERS+is-set}" ] && return
# Set immediately, to avoid the possibility of recursive source'ing.
_MY_SH_SOURCED_ALREADY__PLATFORM_OS_HELPERS=true


# Platform-specific identification

if is_command_found lsb_release
then
    # TODO: Maybe there's a better/safer way that does not use `eval`?
    # Use `eval` to remove any quote syntax that some platforms output.
    # E.g.:
    #   $ println "$(lsb_release -s -i)/$(lsb_release -s -r)"
    #   "NixOS"/"23.05"
    # Versus:
    #   $ println "$(lsb_release -s -i)/$(lsb_release -s -r)"
    #   Ubuntu/22.04
    # Whereas:
    #   $ eval "println $(lsb_release -s -i)/$(lsb_release -s -r)"
    #   NixOS/23.05
    # And:
    #   $ eval "println $(lsb_release -s -i)/$(lsb_release -s -r)"
    #   Ubuntu/22.04
    #
# TODO: Is noisy to stderr on NixOS for some reason, when SHELLOPTS=...:emacs:... was exported.
    eval "MY_PLATFORM_VARIANT=$(lsb_release -s -i)"
    eval "MY_PLATFORM_VERSION=$(lsb_release -s -r)"
fi

readonly MY_PLATFORM_VARIANT MY_PLATFORM_VERSION


# Functions

gnu() {
    std "$@"  # Assume that GNU utilities are the default.
}

_my_terminal_supports_colors() {
    std tput setaf 1 > /dev/null 2>&1  # TODO: Is this portable enough across Linux distros?
}

_my_flock() {
    std flock "$@"
}
