# Wrappers of utils


# shellcheck source=../../../../.local/share/my/gnu/wrappers.bash
if [ -f "$MY_DATA_HOME"/my/gnu/wrappers.bash ]; then
    source "$MY_DATA_HOME"/my/gnu/wrappers.bash || return
fi


function du
{
    ( shopt -s dotglob
      if [ $# -ge 1 ]; then
          local ARGS=("$@")
      else
          local ARGS=(*)
      fi
      command du -s -c -h "${ARGS[@]}" | sort -h )
}


function nix-shell
{
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
        # for some reason).  We evaluate $MY_BASH_INTERACTIVE_CONFIG here,
        # because that variable is not present inside `nix-shell --pure`.
        local MY_COMMAND=("source $(quote "$MY_BASH_INTERACTIVE_CONFIG")/history/init.bash;")

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
