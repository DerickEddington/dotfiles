# This file is loaded, via ~/.profile, by a variety of things that need to have my user's desired
# environment variables.


[ "${_MY_SH_SOURCED_ALREADY__ENV_PROFILE+is-set}" ] && return
# Set immediately, to avoid the possibility of recursive `source`ing.
_MY_SH_SOURCED_ALREADY__ENV_PROFILE=true


umask u+r,go-w  # Ensure these at least, with whatever else a particular host sets-up.

readonly LOGNAME USER  # Why not?


_MY_SH_HELPERS__ONLY_FUNCTIONS=yes  # (Must not be a variable assignment in the next `.` command.)
# shellcheck source=../../../.local/share/my/sh/helpers.sh
. "${XDG_DATA_HOME:-$HOME/.local/share}"/my/sh/helpers.sh
unset _MY_SH_HELPERS__ONLY_FUNCTIONS


if [ "${HOME-}" ]; then  # (Just in case.)

    # If the $HOME pathname involves a symlink, change it to the normalized pathname without
    # symlinks, to help some tools that are confused by $HOME being a symlink.
    #
    _my_normHome=$(norm_abs_path "$HOME")
    if [ "$_my_normHome" != "$HOME" ]; then
        _my_origHome=$HOME
        export HOME="$_my_normHome"
        if [ "$(pwd)" = "$_my_origHome" ]; then
            cd "$HOME"
        fi
    fi
    unset _my_normHome _my_origHome

    readonly HOME  # Why not?


    # XDG-BDS location for executables of per-user-installed applications and facilities
    #
    prepend_to_PATH_if_ok "$HOME"/.local/bin

    # Another location for executables & libraries of per-user-installed applications and
    # facilities
    #
    prepend_to_PATH_if_ok "$HOME"/local/bin
    prepend_to_LD_LIBRARY_PATH_if_ok "$HOME"/local/lib

    # Per-user configuration of user-custom executables
    #
    prepend_bin_and_subs_to_PATH_if_ok "$HOME"/bin

    export PATH
fi


# Finish loading my/sh/helpers.sh, after HOME is finalized.
#
_my_sh_helpers__finish


# Ensure XDG_RUNTIME_DIR is exported
#
if [ "${MY_RUNTIME_DIR-}" ]; then
    if ! [ "${XDG_RUNTIME_DIR-}" ]; then
        export XDG_RUNTIME_DIR="$MY_RUNTIME_DIR"
    fi
else
    warn "MY_RUNTIME_DIR is undefined!"
fi


if [ "${MY_PLATFORM_VARIANT-}" != NixOS ]; then

    export TZ=:US/Pacific  # (The `:` complies with POSIX for implementation-defined manner.)

    if is_command_found emacs ; then
        export EDITOR='emacs --no-window-system'
    elif is_command_found nano ; then
        export EDITOR=nano
    fi
    if [ "${EDITOR-}" ]; then
        export VISUAL="$EDITOR"
    fi
    if is_command_found most ; then
        export PAGER=most
    fi

    # Platform-specific.  This is all ordered intentionally, so that more-specific
    # platform-delegation can take precedence, and so that user-configuration locations can take
    # precedence, and so that further profile.sh files can modify those.  (Maintenance: keep in
    # sync with MY_PLATFORM_IDS in helpers.bash.)
    #
    for _my_platform_id
    in "${MY_PLATFORM_OS-}" "${MY_PLATFORM_OS_VARIANT-}" "${MY_PLATFORM_OS_VAR_VER-}"
    do
        if [ "$_my_platform_id" ]; then

            # If Emacs' TRAMP is getting my user's environment
            #
            case "${INSIDE_EMACS-}" in
                (*tramp*)
                    # Include the GNU utilities in PATH, because my TRAMP configuration assumes
                    # these, and TRAMP cannot use my other wrapper shell functions for these.
                    prepend_to_PATH_if_ok "$MY_DATA_HOME"/my/gnu/platform/"$_my_platform_id"/bin
                    ;;
            esac

            # Locate a user's platform-specific binaries under $XDG_CONFIG_HOME because these are
            # something which each user wants to configure their different ways (as opposed to
            # things that should always be provided for all users).  Note that there can be
            # binaries provided for all users in, e.g.,
            # $XDG_DATA_HOME/my/platform/$_my_platform_id/{bin,local/bin}/ and note that a user
            # can choose which of those to symlink from $_my_platspec_config/bin/ e.g..
            #
            _my_platspec_config="$MY_CONFIG_HOME"/my/platform/"$_my_platform_id"

            # Location for executables & libraries of per-user-installed platform-specific
            # applications and facilities
            #
            prepend_to_PATH_if_ok "$_my_platspec_config"/local/bin
            prepend_to_LD_LIBRARY_PATH_if_ok "$_my_platspec_config"/local/lib

            # Per-user configuration of user-custom platform-specific executables
            #
            prepend_bin_and_subs_to_PATH_if_ok "$_my_platspec_config"/bin

            # Per-user configuration of platform-specific further environment customization
            #
            _my_platspec_profile="$MY_CONFIG_HOME"/my/env/platform/"$_my_platform_id"/profile.sh
            if [ -e "$_my_platspec_profile" ]; then
                # shellcheck source=/dev/null  # (Don't care if there isn't one.)
                . "$_my_platspec_profile"
            fi
        fi
    done
    unset _my_platform_id _my_platspec_config _my_platspec_profile

    export PATH
else
    :  # Assume Home Manager and NixOS are being used to configure these aspects elsewise.
fi
