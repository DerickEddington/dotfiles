# shellcheck disable=SC2034  # These variables are used by what `source`s this file.


# Avoid source'ing this file more than once.
#
[ "${_MY_SH_SOURCED_ALREADY__PLATFORM_HELPERS+is-set}" ] && return
# Set immediately, to avoid the possibility of recursive source'ing.
_MY_SH_SOURCED_ALREADY__PLATFORM_HELPERS=true


# Platform-specific identification

if type lsb_release > /dev/null 2>&1
then
    # TODO: Maybe there's a better/safer way that does not use `eval`?
    # Use `eval` to remove any quote syntax that some platforms output.
    # E.g.:
    #   $ echo "$(lsb_release -s -i)/$(lsb_release -s -r)"
    #   "NixOS"/"23.05"
    # Versus:
    #   $ echo "$(lsb_release -s -i)/$(lsb_release -s -r)"
    #   Ubuntu/22.04
    # Whereas:
    #   $ eval "echo $(lsb_release -s -i)/$(lsb_release -s -r)"
    #   NixOS/23.05
    # And:
    #   $ eval "echo $(lsb_release -s -i)/$(lsb_release -s -r)"
    #   Ubuntu/22.04
    #
    eval "MY_PLATFORM=Linux/$(lsb_release -s -i)/$(lsb_release -s -r)"
else
    MY_PLATFORM=Linux  # TODO: Maybe something else could be done to make it more specific?
fi

MY_PLATFORM_ARCH=$MY_PLATFORM/$(uname -m)

readonly MY_PLATFORM MY_PLATFORM_ARCH


# Functions

_my_terminal_supports_colors() {
    std tput setaf 1 > /dev/null 2>&1  # TODO: Is this portable enough across Linux distros?
}
