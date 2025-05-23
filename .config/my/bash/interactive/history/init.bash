# (Remember: This might also be source'ed by other things or at other times - e.g. by my
#  `nix-shell --pure` wrapper.)
#
# (This logic has a lot of `|| return` to be robust by aborting early, and `|| true` to be robust
#  by continuing, which is helpful when unusual strange circumstances could break this logic,
#  which is helpful to still allow login to continue with such strangeness, versus exiting the
#  shell which would prevent login.)

# shellcheck source=../../../../../.local/share/my/bash/helpers.bash
source "${XDG_DATA_HOME:-$HOME/.local/share}"/my/bash/helpers.bash || return


# If already source'd, don't do anything.
_my_bash_sourced_already config/my/bash/interactive/history/init && return

# All related config files are relative to the current file.
MYSELF_RELDIR=$(std dirname "${BASH_SOURCE[0]}") || return  # (Must be outside any function.)
MY_BASH_HISTORY_CONFIG=$(abs_path "$MYSELF_RELDIR") || return
readonly MY_BASH_HISTORY_CONFIG
unset MYSELF_RELDIR

[ "${MY_STATE_HOME:-}" ] || return
readonly MY_BASH_HISTDIR=$MY_STATE_HOME/my/bash/interactive/history

# Initial creation of the `combined` history file and of the history directory, to make them be
# inaccessible to other users by default.  This must be done before anything else would otherwise
# create them with less-restricted modes.  The owning user may later choose to manually change
# either of their modes, to make them be accessible to other users if desired, and such will be
# kept.
if ! [ -e "$MY_BASH_HISTDIR" ]
then
    std mkdir -p -m go='' "$MY_BASH_HISTDIR" \
    || warn "Unable to create history directory with restricted mode"
fi
if ! [[ -d "$MY_BASH_HISTDIR" && -O "$MY_BASH_HISTDIR"
        && -w "$MY_BASH_HISTDIR" && -x "$MY_BASH_HISTDIR" ]]
then
    error "Bad state of history directory ${MY_BASH_HISTDIR@Q}!"
    return 1
fi
if ! [ -r "$MY_BASH_HISTDIR" ]
then
    warn "Unreadable history directory ${MY_BASH_HISTDIR@Q}"
fi
if ! [ -e "$MY_BASH_HISTDIR"/combined ]
then
    { std touch "$MY_BASH_HISTDIR"/combined \
      && std chmod go='' "$MY_BASH_HISTDIR"/combined ;} \
    || warn "Unable to create \`combined\` history file with restricted mode"
fi
if ! [[ -f "$MY_BASH_HISTDIR"/combined && -O "$MY_BASH_HISTDIR"/combined
        && -w "$MY_BASH_HISTDIR"/combined && ! -x "$MY_BASH_HISTDIR"/combined ]]
then
    error "Bad state of \`combined\` history file!"
    return 1
fi
if ! [ -r "$MY_BASH_HISTDIR"/combined ]
then
    warn "Unreadable \`combined\` history file"
fi

# Use the `combined` history file as the lock file for locking itself.  It's important that the
# lock file be the same file when the $HOME (actually, more precisely: $MY_BASH_HISTDIR) directory
# is shared across multiple hosts.  Modern Linux can lock over NFS or CIFS (SMB), and hopefully
# over other shared file-systems, and so hopefully other unix-like OSs can also.  If locking on a
# shared FS doesn't work in some situations, that's unfortunate, and I'm unsure what exactly will
# happen.
readonly MY_BASH_COMBINED_HISTFILE_LOCK=$MY_BASH_HISTDIR/combined

MY_BASH_HISTORY_COMBINER_IGNORES=$MY_BASH_HISTORY_CONFIG/ignores.regexes
declare -x MY_BASH_HISTORY_COMBINER_IGNORES  # For my-bash_history-combiner utility.


# Append to the history file, don't overwrite it
shopt -s histappend
# Save multi-line commands as one entry.
shopt -s cmdhist


# Separate history file for each Bash session, for keeping an organized archive, and for assisting
# the immediate writing of each command without interleaving such with other instances doing the
# same.  Another good consequence is that things like `nix-shell --pure` that disable `histappend`
# and so wipe-out the $HISTFILE but that don't source ~/.bashrc (e.g. due to using --rcfile)
# cannot wipe-out my main history file because those things will use a different HISTFILE value
# (this is true independently of my extra `nix-shell` shell function).
is-function-undef _my_unique_histfile || return
function _my_unique_histfile {
    local - ; set -o nounset +o errexit
    local ID=$1 FILENAME DIRNAME
    FILENAME=$MY_BASH_HISTDIR/by-time/$(std date +%Y/%m/%d/%T)--${HOSTNAME}${ID:+--}$ID || return
    DIRNAME="$(std dirname "$FILENAME")" || return
    std mkdir -p "$DIRNAME" || return
    FILENAME=$(gnu mktemp "$FILENAME"--XXXXXXXXXX) || return
    print "$FILENAME"
}
declare-function-readonly _my_unique_histfile

if ! [[ "${HISTFILE:-}" = */by-time/* ]]
then
    MY_BASH_SESSION_HISTFILE=$(_my_unique_histfile $$) || return
    # Assign these HIST* once ready - might cause side-effects.
    HISTFILE=$MY_BASH_SESSION_HISTFILE
else
    # HISTFILE was already setup according to my custom scheme, so keep using that.
    MY_BASH_SESSION_HISTFILE=$HISTFILE
fi
# (Note: The per-session history files don't need us to change their modes to be inaccessible to
# other users, because Bash already does that to the $HISTFILE for us.)

# Cause timestamps to be written to the history file, which is also needed for `lithist` to
# preserve single-entry of a multi-line entry.  Also used when the `history` builtin command
# displays the history.
readonly HISTTIMEFORMAT='%F %T %Z:  '


# Aspects that a user might want to customize.
source "$MY_BASH_HISTORY_CONFIG"/config.bash || true


# Mutex the `combined` file, because multiple sessions access it.

std mkdir -p "$(std dirname "$MY_BASH_COMBINED_HISTFILE_LOCK")" || return

is-function-undef _my_lock_combined_histfile || return
function _my_lock_combined_histfile {
    local - ; set -o nounset +o errexit
    local LOCK_FD LOCK_FILE=$MY_BASH_COMBINED_HISTFILE_LOCK

    # Open a new file descriptor of the lock file.  Must be opened for writing so that exclusive
    # locking will work over shared FSs.  Must be opened for appending so that the file is not
    # truncated (clobbered), because it might be the `combined` history file itself.
    exec {LOCK_FD}>> "$LOCK_FILE"

    if _my_flock "${1:-}" --timeout 10 $LOCK_FD ; then
        print $LOCK_FD
    else
        warn "Failed to lock $LOCK_FILE"
        exec {LOCK_FD}>&-  # Close the FD to clean-up.
        return 1
    fi
}
declare-function-readonly _my_lock_combined_histfile


is-function-undef _my_load_combined_histfile || return
function _my_load_combined_histfile {
    local - ; set -o nounset +o errexit
    local LOCK_FD
    if LOCK_FD=$(_my_lock_combined_histfile --shared); then
        history -n "$MY_BASH_HISTDIR"/combined  # (-n seems slightly more appropriate than -r)
        exec {LOCK_FD}>&-  # Close the FD to release the lock.
    fi
}
declare-function-readonly _my_load_combined_histfile

# Start with the past history of combined old sessions.
_my_load_combined_histfile || return


# Immediately write each command to the history file, in case this Bash session has some problem
# and fails to do so when it exits.
PROMPT_COMMAND="${PROMPT_COMMAND:-} ${PROMPT_COMMAND:+;} history -a || true"


# Helper command for when you want a session to not save any of its history.
# Note that `history -a` etc will do nothing, as desired, without a HISTFILE.
is-function-undef no-histfile || return
function no-histfile {
    local - ; set -o nounset +o errexit
    [ "${MY_BASH_SESSION_HISTFILE:-}" ] && std rm "${1:--i}" "$MY_BASH_SESSION_HISTFILE"
    unset HISTFILE MY_BASH_SESSION_HISTFILE
}
declare-function-readonly no-histfile


# Combine the previous `combined` history with this session's and write that as the new `combined`
# history with further ignoring and deduplication, so that the `combined` history file is like a
# database of more-interesting past commands without preserving them all nor their session's
# sequence, whereas a per-session history file is a complete record of all the session's commands
# and preserves their sequence.
is-function-undef _my_histfile_combining || return
function _my_histfile_combining {
    local - ; set -o nounset +o errexit

    [ "${MY_BASH_HISTORY_COMPLETED_COMBINING:-}" ] && return

    history -a || true  # Ensure this session's history file is completed.

    [ "${MY_BASH_SESSION_HISTFILE:-}" ] && [ "${MY_BASH_HISTDIR:-}" ] || return

    if [ -s "$MY_BASH_SESSION_HISTFILE" ]; then
        std chmod a-w "$MY_BASH_SESSION_HISTFILE"  # Protect it as an archive.

        if _my_lock_combined_histfile --exclusive > /dev/null
        then
            local PREV_COMBINED
            PREV_COMBINED=$(gnu mktemp "$MY_BASH_HISTDIR"/combined-prev-XXXXXXXXXX) || return
            std cp "$MY_BASH_HISTDIR"/combined "$PREV_COMBINED" || return
            std chmod a-w "$PREV_COMBINED"  # Might as well.

            if is-command-found my-bash_history-combiner ; then
                # Use it wherever it currently is from.
                local MY_BASH_HISTORY_COMBINER=my-bash_history-combiner

            elif [ "${MY_PLATFORM_VARIANT-}" = NixOS ]; then
                # When it's not in the PATH (e.g. inside `nix-shell --pure`), assume it's here:
                local MY_BASH_HISTORY_COMBINER=~/.nix-profile/bin/my-bash_history-combiner
            else
                local BIN_DIR
                # For all other platforms, assume it's hopefully here:
                if [ "${MY_PLATFORM_OS_VAR_VER_ARCH-}" ]; then
                    BIN_DIR=$(_my_platspec_install_dir)/bin
                else
                    # Or in the XDG-BDS location:
                    BIN_DIR=bin
                fi
                local MY_BASH_HISTORY_COMBINER=~/.local/$BIN_DIR/my-bash_history-combiner
            fi

            std chmod u+w "$MY_BASH_HISTDIR"/combined  # In case it somehow became read-only.

            if $MY_BASH_HISTORY_COMBINER "$PREV_COMBINED" "$MY_BASH_SESSION_HISTFILE" \
                 > "$MY_BASH_HISTDIR"/combined  # Write to original, to preserve inode.
            then
                readonly MY_BASH_HISTORY_COMPLETED_COMBINING=true
            else
                std cp -f "$PREV_COMBINED" "$MY_BASH_HISTDIR"/combined  # Restore if error.
                std chmod u+w "$MY_BASH_HISTDIR"/combined  # In case of `-f` of `cp`.
            fi
            std rm -f "$PREV_COMBINED"

            # When the bash process terminates, it will close the lock FD which will release the
            # lock.
        fi
    else
        no-histfile -f  # If this session had no commands entered, delete its empty history file.
    fi
}
declare-function-readonly _my_histfile_combining

is-function-undef _my_histfile_combining_ignore_failure || return
function _my_histfile_combining_ignore_failure {
    _my_histfile_combining || true
}
declare-function-readonly _my_histfile_combining_ignore_failure

if [ -v IN_NIX_SHELL ]; then
    # Run it via the exitHandler EXIT trap of $stdenv/setup of nix-shell.
    exitHooks+=(_my_histfile_combining_ignore_failure)
elif [ -z "$(trap -p EXIT)" ]; then  # Don't replace any preexisting trap.
    trap _my_histfile_combining_ignore_failure EXIT
else
    warn "Unable to setup _my_histfile_combining for exit."
fi

# Also trap on SIGHUP, because that signal is received when the terminal is forcibly closed.  In
# such case, this is needed to make the EXIT trap work to its completion to do our
# _my_histfile_combining (otherwise Bash kills itself by sending itself another SIGHUP, before it
# finishes the EXIT trap).  This SIGHUP trap causes Bash to run it instead of killing itself,
# before the later exiting, which is why it has an effect, and this enables Bash to complete our
# _my_histfile_combining and then to exit normally.  (Specifically, this causes Bash to install
# and call `trap_handler` upon SIGHUP, instead of `termsig_sighandler` that would cause calling
# `termsig_handler` that would call `kill_shell`.)  Bash still does its exiting upon SIGHUP even
# with a trap for it is registered.
if [ -z "$(trap -p SIGHUP)" ]; then  # Don't replace any preexisting trap.
    trap : SIGHUP
else
    warn "Unable to setup \`:\` trap for SIGHUP."
fi
