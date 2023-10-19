# This file is loaded, via ~/.profile, by a variety of things that need to have my user's desired
# environment variables.


[ "${_MY_SH_SOURCED_ALREADY__ENV_PROFILE+is-set}" ] && return
# Set immediately, to avoid the possibility of recursive `source`ing.
_MY_SH_SOURCED_ALREADY__ENV_PROFILE=true


umask u+r,go-w  # Ensure these at least, with whatever else a particular host sets-up.

readonly LOGNAME USER  # Why not?


if [ "${HOME-}" ]; then  # (Just in case.)

    _MY_SH_HELPERS__ONLY_FUNCTIONS=yes  # (Must not be an assignment in the next `.` command.)
    # shellcheck source=../../../.local/share/my/sh/helpers.sh
    . "${XDG_DATA_HOME:-$HOME/.local/share}"/my/sh/helpers.sh
    unset _MY_SH_HELPERS__ONLY_FUNCTIONS


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


    # Finish loading my/sh/helpers.sh, after HOME is finalized.
    #
    _my_sh_helpers__finish


    # The following locations are for portable platform-independent and architecture-independent
    # executables (i.e. scripts or byte-code, not compiled machine-code), so that they are valid
    # on multiple different OS platforms when the $HOME directory is shared across such.  (This is
    # why these don't also support lib/ sub-directories added to LD_LIBRARY_PATH.)  For
    # platform-specific and architecture-specific locations, see farther below.

    # XDG-BDS location for executables of user-installed applications and facilities that should
    # be considered as being in the ambient environment (i.e. not as something the user is
    # frequently making changes to nor wants to see usually).  Note: ~/.local/ can be an easy base
    # destination for installers (e.g. those that can take something like the --prefix option),
    # but only for installing portable things here.  (A better name than "local" would be
    # "installed", since locality is not the concern, but "local" is supposed to be analogous to
    # /usr/local/ (which has a similarly bad name for its de-facto modern purpose).)
    #
    prepend_to_PATH_if_ok "$HOME"/.local/bin

    # Location for executables (that are portable) as customized by the user.  Including the
    # sub-directories can be especially convenient for symlink'ing like:
    # ~/bin/xyzzy-1.2.3 -> ~/tmp/xyzzy-1.2.3/bin
    #
    prepend_and_subs_to_PATH_if_ok "$HOME"/bin

    export PATH

else
    # Helpers needed by the rest of this file.  These should be like those of helpers.sh.
    warn() { command -p  printf '%s\n' "Warning${1:+: $1}" 1>&2 ;}
    is_command_found() { command -v "${1:-}" > /dev/null 2>&1 ;}

    warn "HOME is undefined!"

    # The rest of this file must avoid trying to use $HOME.  This is currently achieved by the
    # below conditionals like `[ "${MY_*-}" ]` which will be false when helpers.sh was not
    # source'd.
fi


# Ensure XDG_RUNTIME_DIR is exported
#
if [ "${MY_RUNTIME_DIR-}" ]; then
    if ! [ "${XDG_RUNTIME_DIR-}" ]; then
        export XDG_RUNTIME_DIR="$MY_RUNTIME_DIR"
    fi
else
    warn "MY_RUNTIME_DIR is undefined!"
fi

if [ "${XDG_RUNTIME_DIR-}" ]
then
    # Directories for control sockets for multiplexed SSH connections.  This must create the
    # hard-coded directories that ~/.ssh/config uses, and this may create others.
    # shellcheck disable=SC2043
    #
    for _my_control_sockets_subdir in ssh ; do  # (Could add more if needed.)
        std mkdir -p "$XDG_RUNTIME_DIR"/my/"$_my_control_sockets_subdir"
    done
    unset _my_control_sockets_subdir
else
    warn "XDG_RUNTIME_DIR is still undefined!"
fi


# Platform-specific and architecture-specific aspects.  This is all ordered intentionally, so that
# more-specific platform-delegation can take precedence, and so that architecture-specific can
# take precedence, and so that user-customization can take precedence, and so that further
# sub-profile.sh files can modify those.  (Maintenance: Keep in sync with MY_PLATFORM_IDS in
# helpers.bash.)

for _my_platform_id in                                              \
    "${MY_PLATFORM_OS-}"         "${MY_PLATFORM_OS_ARCH-}"          \
    "${MY_PLATFORM_OS_VARIANT-}" "${MY_PLATFORM_OS_VARIANT_ARCH-}"  \
    "${MY_PLATFORM_OS_VAR_VER-}" "${MY_PLATFORM_OS_VAR_VER_ARCH-}"
do
    if [ "$_my_platform_id" ]; then

        # If Emacs' TRAMP is getting my user's environment
        #
        case "${INSIDE_EMACS-}" in
            (*tramp*)
                # Include the GNU utilities in PATH, because my TRAMP configuration assumes these,
                # and TRAMP cannot use my other wrapper shell functions for these.
                #
                prepend_to_PATH_if_ok "$MY_DATA_HOME"/my/gnu/platform/"$_my_platform_id"/bin
                ;;
        esac

        # Locate a user's (platform-specific and architecture-specific) binaries under this
        # pathname scheme, because the `my` prefix helps avoid name conflicts in the various
        # parent directories, since those directories may also be used by arbitrary other things
        # to contain arbitrary other names.
        #
        _my_platspec=my/platform/"$_my_platform_id"

        # Locations for executables & libraries of user-installed (platform-specific and
        # architecture-specific) applications and facilities that should be considered as being in
        # the ambient environment (i.e. not as something the user is frequently making changes to
        # nor wants to see usually).  Note: this can be an easy base destination for installers
        # (e.g. those that can take something like:
        # --exec-prefix=~/.local/my/platform/Linux/Ubuntu/22.04/x86_64/installed).  The
        # `installed` suffix helps avoid name conflicts in the nested platform directories, since
        # those directories may also contain arbitrary other names (for platform variants,
        # versions, and architectures), and we can't control what names arbitrary installers will
        # create in a base destination.  (These are under ~/.local/ because these are for the same
        # purpose as that.)  (Maintenance: Keep in sync with: _my_platspec_install_dir.)
        #
        prepend_to_PATH_if_ok            "$HOME"/.local/"$_my_platspec"/installed/bin
        prepend_to_LD_LIBRARY_PATH_if_ok "$HOME"/.local/"$_my_platspec"/installed/lib

        # Locations for executables & libraries (platform-specific and architecture-specific) as
        # customized by the user.  The `bin` and `lib` suffixes are very unlikely to ever conflict
        # with the name of a platform variant, version, or architecture.  (These are under ~/bin/
        # & ~/lib/ because these are for the same purpose as those.)  Including the
        # sub-directories can be especially convenient for symlink'ing like:

        # ~/lib/my/platform/Linux/lib/acme-3.2.1 -> ~/tmp/acme-3.2.1/lib
        #
        prepend_and_subs_to_PATH_if_ok            "$HOME"/bin/"$_my_platspec"/bin
        prepend_and_subs_to_LD_LIBRARY_PATH_if_ok "$HOME"/lib/"$_my_platspec"/lib

        # Further customization of platform-specific and architecture-specific aspects of the
        # user's environment.  These should guard against the possibility of being source'd
        # multiple times.
        #
        _my_platspec_profile="$MY_CONFIG_HOME"/my/env/platform/"$_my_platform_id"/profile.sh
        if [ -f "$_my_platspec_profile" ]; then
            # shellcheck source=/dev/null  # (Don't care if there isn't one.)
            . "$_my_platspec_profile"
        fi
    fi
done
unset _my_platform_id _my_platspec _my_platspec_profile

export PATH

if [ "${MY_PLATFORM_OS_VAR_VER_ARCH-}" ]
then
    # What `cargo install` installs is OS-specific, sometimes platform-variant-specific, and
    # architecture-specific, so place such according to my scheme (which will be automatically
    # added to PATH).  But CARGO_HOME and RUSTUP_HOME are not specific, so let them default to
    # ~/.cargo/ and ~/.rustup/ so they can be shared when $HOME is used across multiple hosts.
    #
    CARGO_INSTALL_ROOT=$(_my_platspec_install_dir) && export CARGO_INSTALL_ROOT
fi


# Miscellaneous

export TZ=:America/Los_Angeles  # (The `:` complies with POSIX for implementation-defined.)

if [ "${MY_PLATFORM_VARIANT-}" = NixOS ]
then
    :  # Assume that Home Manager and NixOS are being used to configure these aspects elsewise.
else
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
fi
