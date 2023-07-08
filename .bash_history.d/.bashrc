# Append to the history file, don't overwrite it
shopt -s histappend
# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
# Don't expand !
set +o histexpand
# Save multi-line commands as one entry.
shopt -s cmdhist
# Save multi-line commands as multi-line (instead of as a single line converted to have ";").
shopt -s lithist

# Separate history file for each Bash session, for keeping an organized archive, and for assisting
# the immediate writing of each command without interleaving such with other instances doing the
# same.  Another good consequence is that things like `nix-shell --pure` that disable `histappend`
# and so wipe-out the $HISTFILE but that don't source ~/.bashrc (e.g. due to using --rcfile)
# cannot wipe-out my main history file because those things will use a different HISTFILE value
# (this is true independently of my extra `nix-shell` shell function).
function _my_unique_histfile {
    local ID=$1
    local FILENAME=~/.bash_history.d/by-time/$(date +%Y/%m/%d/%T)--${HOSTNAME}${ID:+--}$ID
    mkdir -p "$(dirname "$FILENAME")"
    if type -t mktemp >& /dev/null; then
        FILENAME=$(mktemp "$FILENAME"--XXXXXXXXXX)
    fi
    echo "$FILENAME"
}

MY_SESSION_HISTFILE=$(_my_unique_histfile $$)
# Assign these HIST* once ready - might cause side-effects.
HISTFILE="$MY_SESSION_HISTFILE"
# Practically unlimited, but limited against madness.
HISTFILESIZE=10000000
# Limit a mad session's history to be small enough to not clobber much of the `combined` file.
HISTSIZE=$((HISTFILESIZE / 100))
# Don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth
# Don't put commands matching these in the history.
HISTIGNORE=\\:  # The `:` command by itself.
# Cause timestamps to be written to the history file, which is also needed for
# `lithist` to preserve single-entry of a multi-line entry.  Also used when the
# `history` builtin command displays the history.
HISTTIMEFORMAT='%F %T %Z:  '

# Mutex the `combined` file, because multiple sessions access it.
function _my_lock_combined_histfile {
    local LOCK_FD LOCK_FILE=~/.bash_history.d/combined.lock
    exec {LOCK_FD}>> $LOCK_FILE  # Open a new file descriptor of it.
    if flock ${1:-} --timeout 10 $LOCK_FD ; then
        echo $LOCK_FD
    else
        echo "Failed to lock $LOCK_FILE" >&2
        exec {LOCK_FD}>&-  # Close the FD to clean-up.
        return 1
    fi
}

function _my_load_combined_histfile {
    local LOCK_FD=$(_my_lock_combined_histfile --shared)
    if (($? == 0)); then
        history -n ~/.bash_history.d/combined  # (-n seems slightly more appropriate than -r would)
        exec {LOCK_FD}>&-  # Close the FD to release the lock.
    fi
}

# Start with the past history of combined old sessions.
_my_load_combined_histfile

# Immediately write each command to the history file, in case this Bash session has some problem
# and fails to do so when it exits.
PROMPT_COMMAND="${PROMPT_COMMAND:-} ${PROMPT_COMMAND:+;} history -a || true"

# Helper command for when you want a session to not save any of its history.
# Note that `history -a` etc will do nothing, as desired, without a HISTFILE.
function no-histfile {
    [ "$MY_SESSION_HISTFILE" ] && command rm ${1:--i} "$MY_SESSION_HISTFILE"
    unset HISTFILE MY_SESSION_HISTFILE
}

# Combine the previous `combined` history with this session's and write that as the new `combined`
# history with further ignoring and deduplication, so that the `combined` history file is like a
# database of more-interesting past commands without preserving them all nor their session's
# sequence, whereas a per-session history file is a complete record of all the session's commands
# and preserves their sequence.
function _my_histfile_combining {
    history -a || true  # Ensure this session's history file is completed.

    if [ -s "$MY_SESSION_HISTFILE" ]; then
        command chmod a-w "$MY_SESSION_HISTFILE"  # Protect it as an archive.

        if _my_lock_combined_histfile --exclusive > /dev/null
        then
            [ ! -e ~/.bash_history.d/combined ] && command touch ~/.bash_history.d/combined
            local PREV_COMBINED=$(command mktemp ~/.bash_history.d/combined-prev-XXXXXXXXXX)
            command cp ~/.bash_history.d/combined "$PREV_COMBINED"

            if ! my-bash_history-combiner "$PREV_COMBINED" "$MY_SESSION_HISTFILE" \
                   > ~/.bash_history.d/combined  # Write to original, to preserve inode.
            then
                command cp -f "$PREV_COMBINED" ~/.bash_history.d/combined  # Restore if error.
            fi
            command rm -f "$PREV_COMBINED"

            # When the bash process terminates, it will close the lock FD which will release the
            # lock.
        fi
    else
        no-histfile -f  # If this session had no commands entered, delete its empty history file.
    fi
}
trap _my_histfile_combining EXIT
