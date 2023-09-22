# This file is executed for interactive Bash shells.
#
# (This logic has a lot of `|| return` to be robust by aborting early, and `|| true` to be robust
#  by continuing, which is helpful when unusual strange circumstances could break this logic,
#  which is helpful to still allow login to continue with such strangeness, versus exiting the
#  shell which would prevent login.)

# shellcheck disable=SC2034  # These variables are used by the things that source this.


# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac


# Used by the current and sub init files, and could be useful to the user.
# shellcheck source=../../../../.local/share/my/bash/helpers.bash
source "${XDG_DATA_HOME:-$HOME/.local/share}"/my/bash/helpers.bash || return


# If already source'd, don't do anything.
_my_bash_sourced_already config/my/bash/interactive/init && return


# All related config files are relative to the current file.
MYSELF_RELDIR=$(std dirname "${BASH_SOURCE[0]}") || return  # (Must be outside any function.)
MY_BASH_INTERACTIVE_CONFIG=$(abs_path "$MYSELF_RELDIR") || return
MY_BASH_CONFIG=$(std dirname "$MY_BASH_INTERACTIVE_CONFIG") || return
readonly MY_BASH_INTERACTIVE_CONFIG MY_BASH_CONFIG
unset MYSELF_RELDIR


# Try to use newer bash version if the default one is ancient.
if [ "${BASH_VERSINFO[0]}" -le 3 ] && [ $SHLVL -eq 1 ]
then
    # (The PLATFORM variable is defined in $XDG_CONFIG_HOME/my/env/profile.sh and is available
    # here because that file was source'd before this one.)

    # If PLATFORM undefined, $HOME//... will be used.
    NEW_SH="$HOME/$PLATFORM/local/bin/bash"

    if [ -x "$NEW_SH" ]; then
        export SHELL="$NEW_SH"
        exec "$NEW_SH"
    fi
fi


# Execute the below when any bash is run, whether top-level or not.

# Aspects that a user might want to customize.
source "$MY_BASH_INTERACTIVE_CONFIG"/config.bash || true

# History
source "$MY_BASH_INTERACTIVE_CONFIG"/history/init.bash || true

# My custom prompt.
if [ "${BASH_VERSINFO[0]}" -ge 4 ]; then
    source "$MY_BASH_INTERACTIVE_CONFIG"/prompt.bash || true
fi

# Platform-specific.
if [ -f "$MY_BASH_INTERACTIVE_CONFIG"/platform/"$(uname)"/init.bash ]; then
    # shellcheck source=./platform/FreeBSD/init.bash  #  (Just one of many, to have something.)
    source "$MY_BASH_INTERACTIVE_CONFIG"/platform/"$(uname)"/init.bash || true
fi

# Wrappers of utils, before source'ing aliases.bash (because those can use these).
if [ -f "$MY_BASH_INTERACTIVE_CONFIG"/wrappers.bash ]; then
    source "$MY_BASH_INTERACTIVE_CONFIG"/wrappers.bash || true
fi

# Alias definitions.
if [ -f "$MY_BASH_INTERACTIVE_CONFIG"/aliases.bash ]; then
    source "$MY_BASH_INTERACTIVE_CONFIG"/aliases.bash || true
fi


# Only execute this when a top-level bash is run.  I.e. not for any bash processes run as children
# of another bash.  This prevents things like repeated adding of the same value to PATH.
if [ $SHLVL -eq 1 ]; then
    :
fi
