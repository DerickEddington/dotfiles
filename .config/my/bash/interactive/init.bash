# This file is executed for interactive Bash shells.
#
# (This logic has a lot of `|| return` to be robust by aborting early, and `|| true` to be robust
#  by continuing, which is helpful when unusual strange circumstances could break this logic,
#  which is helpful to still allow login to continue with such strangeness, versus exiting the
#  shell which would prevent login.)

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac


# All related config files are relative to the current file.
MYSELF_RELDIR=$(command -p  dirname "${BASH_SOURCE[0]}") || return  # (Must be outside any function.)
MY_BASH_INTERACTIVE_CONFIG=$(command -p  realpath -m -L -s "$MYSELF_RELDIR") || return
MY_BASH_CONFIG=$(command -p  dirname "$MY_BASH_INTERACTIVE_CONFIG") || return
unset MYSELF_RELDIR


# Try to use newer bash version if the default one is ancient.
if [ "${BASH_VERSINFO[0]}" -le 3 ] && [ $SHLVL -eq 1 ]
then
    # (The PLATFORM variable is defined in $XDG_CONFIG_HOME/my/env/profile.sh
    # and is available here because that file was source'd before this one.)

    # If PLATFORM undefined, $HOME//... will be used.
    NEW_SH="$HOME/$PLATFORM/local/bin/bash"

    if [ -x "$NEW_SH" ]; then
        export SHELL="$NEW_SH"
        exec "$NEW_SH"
    fi
fi


# Execute the below when any bash is run, whether top-level or not.


# Shell options
shopt -s autocd
shopt -s cdspell
shopt -s checkhash
shopt -s checkjobs
shopt -s dirspell
#shopt -s dotglob
shopt -s extglob
# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize
# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# For the `time` builtin command.  Like the default but with the CPU% added.
TIMEFORMAT=$'\nreal\t%3lR\nuser\t%3lU\nsys\t%3lS\nCPU%%\t%P'


# History
source "$MY_BASH_INTERACTIVE_CONFIG"/history/init.bash || true


# My custom prompt.
if [ "${BASH_VERSINFO[0]}" -ge 4 ]; then
    source "$MY_BASH_INTERACTIVE_CONFIG"/prompt.bash || true
fi


# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
if [ -f "$MY_BASH_INTERACTIVE_CONFIG"/aliases.bash ]; then
    source "$MY_BASH_INTERACTIVE_CONFIG"/aliases.bash || true
fi

# Wrappers of utils.
if [ -f "$MY_BASH_INTERACTIVE_CONFIG"/wrappers.bash ]; then
    source "$MY_BASH_INTERACTIVE_CONFIG"/wrappers.bash || true
fi


# Only execute this when a top-level bash is run.  I.e. not for any bash
# processes run as children of another bash.  This prevents things like repeated
# adding of the same value to PATH.
if [ $SHLVL -eq 1 ]; then
    :
fi
