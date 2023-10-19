#! /usr/bin/env sh

readonly HOME  # Guarantee this remains the same for start1.bash.

# Similar to _my_script_prelude.  Done manually here, because we can't source my/sh/helpers.sh
# yet (because we want to use topDir to do that but it needs this).
#
set -e -u  # -o errexit -o nounset
readonly self="$0"
selfDir=$(command -p  dirname "$self")
readonly selfDir
case "$selfDir" in
    (/*) selfDirAbs="$selfDir" ;;
    (*)  selfDirAbs=$(command -p  pwd -L); selfDirAbs=${selfDirAbs%/}/$selfDir ;;
esac
readonly selfDirAbs
selfDirNorm=$(command -p  cd -P -- "$selfDirAbs"   &&   command -p  pwd -P)
readonly selfDirNorm

[ "${VERBOSE:=0}" -ge 5 ] && set -x
[ "${VERBOSE:=0}" -ge 6 ] && set -v

# The top of our same instance of our facility's files.
#
topDir=$selfDirNorm/../../../../..
topDir=$(command -p  cd -P -- "$topDir"   &&   command -p  pwd -P)
readonly topDir


prepare_special_xdg_bds()
{
    # Ensure we use our same instance of our facility's files, because they should be guaranteed
    # to be mutually coherent.
    #
    export   XDG_DATA_HOME="$topDir"/.local/share
    export XDG_CONFIG_HOME="$topDir"/.config

    [ -d "$XDG_DATA_HOME" ] || return
    # It's OK if $XDG_CONFIG_HOME doesn't exist.

    # Must know what this location is before used next.  (Which is why we make it ourself, if
    # needed, instead of letting helpers.sh.)
    #
    if ! [ -d "${XDG_RUNTIME_DIR:-}" ]; then
        XDG_RUNTIME_DIR=$(_my_make_runtime_dir_in_tmp) || return
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


_MY_SH_HELPERS__ONLY_FUNCTIONS=yes  # (Must not be a variable assignment in the next `.` command.)
# shellcheck source=../../sh/helpers.sh
. "$topDir"/.local/share/my/sh/helpers.sh
unset _MY_SH_HELPERS__ONLY_FUNCTIONS

# These environment variables must be set before calling _my_sh_helpers__finish.  This
# prepare_special_xdg_bds can't work until after the above source'ing of my/sh/helpers.sh was done
# (because prepare_special_xdg_bds needs _my_make_runtime_dir_in_tmp).
#
prepare_special_xdg_bds

# Finish loading my/sh/helpers.sh
#
_my_sh_helpers__finish

# Install Bash if not already.  This can't work until after _my_sh_helpers__finish was done.
#
_my_install_bash_if_needed

# Use Bash for the rest.  Remove SHELL from the environment so that Bash will set it to the
# user's login shell, for start1.bash.
#
unset -v SHELL
exec bash "$selfDir"/start1.bash "$@"
