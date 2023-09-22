#! /usr/bin/env sh
set -e -u
[ "${VERBOSE:=0}" -ge 3 ] && set -x
[ "${VERBOSE:=0}" -ge 4 ] && set -v

# Capture these before anything else could mess with them.
#
readonly HOME  # Guarantee this remains the same for `start1.bash`.
selfDir=$(command -p dirname "$0")
selfDir=$(cd "$selfDir" && pwd)  # Absolute pathname via `pwd`.
readonly selfDir
topDir=$(cd "$selfDir"/../../../../.. && pwd)  # Normalized pathname via `cd`.
readonly topDir


make_tmp_runtime_dir() {
    ( # In a subshell to prevent this source'ing from affecting the outer shell.
      # shellcheck source=../../sh/helpers.sh
      . "${XDG_DATA_HOME:?}"/my/sh/helpers.sh > /dev/null 2>&1 || exit
      _my_make_runtime_dir_in_tmp
    )
}

prepare_special_xdg_bds() {
    # Ensure we use our same instance of our facility's files, because they should be guaranteed
    # to be mutually coherent.
    #
    export   XDG_DATA_HOME="$topDir"/.local/share
    export XDG_CONFIG_HOME="$topDir"/.config

    [ -d "$XDG_DATA_HOME" ] || return
    # It's OK if XDG_CONFIG_HOME is not present.

    # Must know what this location is before used next.  (Which is why we make it ourself, if
    # needed, instead of letting helpers.sh.)
    #
    if ! [ -d "${XDG_RUNTIME_DIR:-}" ]; then
        XDG_RUNTIME_DIR=$(make_tmp_runtime_dir) || return  # (After XDG_DATA_HOME was set.)
    fi
    export XDG_RUNTIME_DIR

    # Ensure we do not affect the user's normal locations.
    #
    export XDG_STATE_HOME="$XDG_RUNTIME_DIR"/my/deploy-setup/bootstrap/.local/state
    export XDG_CACHE_HOME="$XDG_RUNTIME_DIR"/my/deploy-setup/bootstrap/.cache
    # shellcheck disable=2174
    command -p  mkdir -p -m u=rwX,g=,o= "$XDG_STATE_HOME" "$XDG_CACHE_HOME" || return

    # Might as well.
    readonly XDG_DATA_HOME XDG_CONFIG_HOME XDG_RUNTIME_DIR XDG_STATE_HOME XDG_CACHE_HOME
}


# These environment variables must be set before source'ing helpers.sh.
#
prepare_special_xdg_bds

# Just in case something inadvertently writes files in the current working directory.
#
# shellcheck disable=2174
command -p  mkdir -p -m u=rwX,g=,o= "$XDG_RUNTIME_DIR"/my/deploy-setup/bootstrap/tmp
cd "$XDG_RUNTIME_DIR"/my/deploy-setup/bootstrap/tmp

# shellcheck source=../../sh/helpers.sh
. "${XDG_DATA_HOME:?}"/my/sh/helpers.sh

# Install Bash if not already.
#
_my_install_bash_if_needed

# Use Bash for the rest.  Remove SHELL from the environment, so that Bash will set it to the
# user's login shell.
#
unset -v SHELL
exec bash "$selfDir"/start1.bash "$@"
