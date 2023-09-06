# This file is executed for interactive Bash shells.

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac


MY_BASH_CONFIG=${XDG_CONFIG_HOME:-~/.config}/my/bash


# Try to use newer bash version if the default one is ancient.
if [ ${BASH_VERSINFO[0]} -le 3  -a  $SHLVL -eq 1 ]
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
source "$MY_BASH_CONFIG"/interactive/history/init.bash


# My custom prompt.
if [ ${BASH_VERSINFO[0]} -ge 4 ]; then
    source "$MY_BASH_CONFIG"/interactive/prompt.bash
fi


# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
if [ -f "$MY_BASH_CONFIG"/interactive/aliases.bash ]; then
    source "$MY_BASH_CONFIG"/interactive/aliases.bash
fi


# Wrappers of utils

function du {
    ( shopt -s dotglob
      if [ $# -ge 1 ]; then
          local ARGS=("$@")
      else
          local ARGS=(*)
      fi
      command du -s -c -h "${ARGS[@]}" | sort -h )
}

function nix-shell {
    local IS_INTERACTIVE=true IS_PURE=false GIVEN_COMMAND="" GIVEN_COMMAND_IDX=""

    # Scan the arguments to see what we're dealing with.
    local I ARGS=("$@")
    for ((I=0; I < ${#ARGS[@]}; I++)); do
        # (Note: This is imperfect and will misinterpret if these patterns are
        #  actually the value of some other option.)
        case "${ARGS[I]}" in
            (--command)
                GIVEN_COMMAND_IDX=$((I + 1))
                GIVEN_COMMAND="${ARGS[GIVEN_COMMAND_IDX]}"
                ;;
            (--run) IS_INTERACTIVE=false ;;
            (--pure) IS_PURE=true ;;
        esac
    done

    if $IS_PURE && $IS_INTERACTIVE; then
        # Use my custom history-file configuration and handling with `nix-shell
        # --pure`.  Also, prevent `nix-shell --pure` from clobbering the default
        # HISTFILE (which is an extra precaution redundantly in addition to
        # other sessions usually using $MY_BASH_HISTDIR/combined and
        # HISTFILE=$MY_BASH_SESSION_HISTFILE, just in case that is not in effect
        # for some reason).  We evaluate $MY_BASH_CONFIG here, because that
        # variable is not present inside `nix-shell --pure`.
        local MY_COMMAND=("source '$MY_BASH_CONFIG'/interactive/history/init.bash;")

        if [ "$GIVEN_COMMAND_IDX" ]; then
            # Allow the given command to control whether it does a "return".
            # This requires that it be placed at the end of MY_COMMAND.
            MY_COMMAND+=("$GIVEN_COMMAND")
            ARGS[GIVEN_COMMAND_IDX]="${MY_COMMAND[*]}"  # Replace the original.
        else
            MY_COMMAND+=("return")  # Drop into the interactive shell.
            ARGS=(--command "${MY_COMMAND[*]}" "${ARGS[@]}")
        fi

        command nix-shell "${ARGS[@]}"
    else
        command nix-shell "$@"  # Don't affect any of the arguments.
    fi
}


# Only execute this when a top-level bash is run.  I.e. not for any bash
# processes run as children of another bash.  This prevents things like repeated
# adding of the same value to PATH.
if [ $SHLVL -eq 1 ]; then
    :
fi
