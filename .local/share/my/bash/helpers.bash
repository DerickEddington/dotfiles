# General helpers for both scripted and interactive shell use.
# Source'ing this must be idempotent, in case it's ever source'd multiple times.

# shellcheck disable=SC2034  # These variables are used by the things that source this.


# Avoid source'ing this file more than once.

if ! declare -g -p -F _my_bash_sourced_already >& /dev/null  # Not defined yet.
then
    function _my_bash_sourced_already
    {
        declare -g -A _MY_BASH_SOURCED_ALREADY || exit

        if [[ -v _MY_BASH_SOURCED_ALREADY["${1:?}"] ]]; then
            return 0  # Enables `_my_bash_sourced_already ... && return` to return 0.
        else
            # Set immediately, to avoid the possibility of recursive source'ing.
            _MY_BASH_SOURCED_ALREADY["$1"]=true
        fi
        return 1
    }
    declare -g -f -r _my_bash_sourced_already || exit  # Make it readonly.
fi

_my_bash_sourced_already local/share/my/bash/helpers && return


# Also provide all of these.
source "$(command -p  dirname "${BASH_SOURCE[0]}")"/../sh/helpers.sh  # (Must not be in function.)


# For defining functions.  It's assumed that it's OK to redefine functions that didn't use these.

! declare -g -p -F is-function-def >& /dev/null || return  # Must not be defined yet.
function is-function-def {
    declare -g -p -F -- "${1:?}" >& /dev/null || return
}

! is-function-def is-function-undef || return
function is-function-undef {
    ! is-function-def "$@"
}

is-function-undef declare-function-readonly-lax || return
function declare-function-readonly-lax {
    declare -g -f -r -- "${1:?}" || return
}

is-function-undef declare-function-readonly || return
function declare-function-readonly {
    declare-function-readonly-lax "$@" || exit
}

declare-function-readonly is-function-def
declare-function-readonly is-function-undef
declare-function-readonly declare-function-readonly-lax
declare-function-readonly declare-function-readonly


# For robustness

# Already defined in ../sh/helpers.sh.
declare-function-readonly std
declare-function-readonly is_command_found
declare-function-readonly _my_script_prelude

is-function-undef is-command-found || return
function is-command-found { is_command_found "$@" ;}
declare-function-readonly is-command-found

is-function-undef _my-script-prelude || return
function _my-script-prelude {
    shopt -s assoc_expand_once extglob globstar nullglob
    _my_script_prelude
}
declare-function-readonly _my-script-prelude

is-function-undef is-var-attr || return
function is-var-attr {
    local - ; set +o nounset
    (( $# == 2 )) || return 2
    [[ "${!1@a}" = *$2* ]]
}
declare-function-readonly is-var-attr

is-function-undef is-var-assoc-array || return
function is-var-assoc-array {
    is-var-attr "$1" A
}
declare-function-readonly is-var-assoc-array

is-function-undef split-on-words || return
function split-on-words {
    (( $# == 1 || $# == 2 )) || return
    IFS=" " read -r -a "${2:-WORDS_SPLITTED}" <<< "$1"
}
declare-function-readonly split-on-words

is-function-undef is-single-word || return
function is-single-word {
    local - ; set -o nounset
    local splitted
    (( $# == 1 )) || return
    split-on-words "$1" splitted
    (( ${#splitted[@]} == 1 ))
}
declare-function-readonly is-single-word

is-function-undef unprefix-cmd || return
function unprefix-cmd {
    local - ; set -o nounset
    local prefix=${1:-} cmd=${2:-}  # (Ignore ${@:3})

    if [[ "$cmd" =~ ^[[:space:]]*([[:alnum:]_].*)$ ]]; then
        cmd=${BASH_REMATCH[1]}
        [[ "$cmd" =~ (.*[^[:space:]]+)[[:space:]]*$ ]] || exit  # Trim trailing whitespace.
        cmd=${BASH_REMATCH[1]}

        if (( $# >= 3 )); then  # Given command is multiple arguments.
            if [ "$prefix" = "$cmd" ]; then
                if [ "${3:-}" ]; then
                    print "${@:3}"  # Without the prefix.
                    return 0
                else
                    return 2  # Invalid. No output.
                fi
            else
                print "${@:2}"  # Unaltered, because it didn't match.
                return 1
            fi
        else  # Given command is single-string argument.
            if [[ "$cmd" =~ ^$prefix([[:space:]]+(.*))?$ ]]; then
                cmd=${BASH_REMATCH[2]}
                if [ "$cmd" ]; then
                    print "$cmd"  # Without the prefix.
                    return 0
                else
                    return 2  # Invalid. No output.
                fi
            else
                print "$cmd"  # Unaltered, because it didn't match.
                return 1
            fi
        fi
    else
        return 2  # Invalid. No output.
    fi
}
declare-function-readonly unprefix-cmd


# Remote hosts

is-function-undef remote-shell || return
function remote-shell
{
    local - ; set -o nounset
    (( $# >= 1 )) || return
    local -r remoteUrl=$1 schemeOpts=("${@:3}")
    if [ "${2:-}" ]; then
        local -r cmd=("$2")
    else
        local -r cmd=()
    fi
    if is_stdin_a_tty ; then local -r sshReqTTY=(-t); else local -r sshReqTTY=(); fi
    local -r sshSepStderr=(${MY_REMOTE_SHELL__SEPARATE_STDERR+-T})

    if [[ "$remoteUrl" =~ ^ssh://[^/]+$ ]]
    then
        # shellcheck disable=SC2029  # Want these expanded client-side.
        ssh -o LogLevel=ERROR "${sshReqTTY[@]}" "${sshSepStderr[@]}" "${schemeOpts[@]}" \
            "$remoteUrl" "${cmd[@]}"

    elif [[ "$remoteUrl" =~ ^vagrant://([^/]+)$ ]]
    then
        local -r machine=${BASH_REMATCH[1]}
        local i vagOpts=() sshOpts=(-- "${sshSepStderr[@]}")  # (`vagrant ssh` does -t as needed.)

        if (( ${#cmd[@]} >= 1 )); then
            vagOpts+=(-c "${cmd[*]}")
        fi
        for (( i = 0; i < ${#schemeOpts[@]}; i++ )); do
            local o="${schemeOpts[i]}"
            if [ "$o" = "--" ]; then
                sshOpts+=("${schemeOpts[@]:i+1}")
                break
            else
                vagOpts+=("$o")
            fi
        done

        vagrant ssh "${vagOpts[@]}" "$machine" "${sshOpts[@]}"

    elif [ "$remoteUrl" = shell://localhost ]  # Occasionally useful (e.g. for testing).
    then
        local localCmd=()
        if (( ${#cmd[@]} >= 1 )); then
            localCmd=(-c "${cmd[*]}")
        fi
        "${SHELL:?}" "${schemeOpts[@]}" "${localCmd[@]}"
    else
        error "remote-shell: Unsupported URL: ${remoteUrl@Q}"
        return 1
    fi
}
declare-function-readonly remote-shell

is-function-undef _remote-shell-specific || return
function _remote-shell-specific {
    local -r shell=${1:?} remoteUrl=${2?} shellCmd=${3:-} schemeOpts=("${@:4}")

    if [ "${shellCmd:-}" ]; then
        # Have the desired remote shell read the given commands from a file in the remote host,
        # instead of trying to pass the given commands on the remote login-shell's command-line
        # (-c ...), because this avoids problems with incompatibilities in the syntax/language of
        # the login shell which will interpret our synthesized command that invokes the desired
        # shell.  E.g. the `csh` login-shell of FreeBSD requires \-line-continuations even inside
        # single-quoted strings, but Bash and POSIX Shell do not, and trying to reconcile such
        # syntax differences is impractical.
        local cmdFile
        # (Use `mktemp` in the local host, because it might not be available in a remote host.)
        cmdFile=$(gnu mktemp -u ./cmdFile-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX) || return

        # Make the $cmdFile unlink itself ASAP, while still executing.
        local -r shellCmdRm="rm -f $cmdFile"$'\n'"$shellCmd"

        # We assume whatever remote login-shell supports the common basic shell language of the
        # commands we synthesize here.
        #
        if remote-shell "$remoteUrl" "cat > $cmdFile" "${schemeOpts[@]}" \
                        <<< "$shellCmdRm" > /dev/null
        then
            remote-shell "$remoteUrl" "$shell $cmdFile" "${schemeOpts[@]}"
        else
            error "_remote-shell-specific: Failed to copy \`cmdFile\`!"
            return 64
        fi
    else
        remote-shell "$remoteUrl" "$shell" "${schemeOpts[@]}"
    fi
}
declare-function-readonly _remote-shell-specific

is-function-undef remote-sh || return
function remote-sh { _remote-shell-specific sh "$@" ;}
declare-function-readonly remote-sh

is-function-undef remote-bash || return
function remote-bash { _remote-shell-specific bash "$@" ;}
declare-function-readonly remote-bash

is-function-undef _remote-shell-specific-cd || return
function _remote-shell-specific-cd {
    # shellcheck disable=SC2016
    local -r dirExpr=${3-'"$HOME"'} inDirCmd=${4:-}
    local -r cdCmd='( cd '"$dirExpr"' < /dev/null > /dev/null || exit
                      '"$inDirCmd"'
                    )'
    _remote-shell-specific "$1" "$2" "$cdCmd" "${@:5}"
}
declare-function-readonly _remote-shell-specific-cd

is-function-undef remote-sh-cd || return
function remote-sh-cd { _remote-shell-specific-cd sh "$@" ;}
declare-function-readonly remote-sh-cd

is-function-undef remote-bash-cd || return
function remote-bash-cd { _remote-shell-specific-cd bash "$@" ;}
declare-function-readonly remote-bash-cd


# Miscellaneous

is-function-undef prepend-to-file || return
function prepend-to-file {
    local -r fileName="${1:?}" content="${2:?}"
    local tf ; tf=$(gnu mktemp "$fileName"--XXXXXXXXXXXXXXXX) || return ; readonly tf

    print "$content" > "$tf" || return
    if [ -e "$fileName" ]; then
        std cat "$fileName" >> "$tf" || return
    fi
    std mv -f "$tf" "$fileName"
}
declare-function-readonly prepend-to-file

is-function-undef append-to-file || return
function append-to-file {
    local -r fileName="${1:?}" content="${2:?}"
    print "$content" >> "$fileName"
}
declare-function-readonly append-to-file

function _my-download-https
{
    # Write to standard output the content of a fetched HTTPS URL.
    local -r url=${1:?}

    if is-command-found curl ; then
        local -r download=(curl --proto '=https' --tlsv1.2 --fail --silent --show-error --location)
    elif is-command-found wget ; then
        local -r download=(wget --https-only --secure-protocol=TLSv1_2 --no-verbose -O -)
    else
        error "Can't find a tool to download with!"
        return 1
    fi

    "${download[@]}" "$url" || {
        error "Failed to download ${url@Q}!"
        return 2
    }
}

function remove-dups
{
    # Remove duplicate elements from an array variable given by name.

    local -n -r arrayVarNameRef=${1:?}
    local -r orig=("${arrayVarNameRef[@]}")

    if shopt -q assoc_expand_once ; then
        local -r resetAEO=no  # It's already set.
    else
        local -r resetAEO=yes
        shopt -s assoc_expand_once  # Critical for correctness of `seen` keys! Insane!
    fi

    arrayVarNameRef=()  # Clear it, to reaccumulate only non-duplicates.

    local -A seen
    local elem
    for elem in "${orig[@]}" ; do
        if ! [[ -v seen["$elem"] ]]; then
            arrayVarNameRef+=("$elem")  # Not duplicate. Accumulate it.
            seen["$elem"]=yes
        fi
    done

    if [ "$resetAEO" = yes ]; then
        shopt -u assoc_expand_once
    fi
}

function git-clone-into-nonempty
{
    local - ; set -o nounset

    (($# >= 2))
    local ARGS=("$@")
    local OPTS=("${ARGS[@]:0:$#-2}")
    local REPO="${ARGS[-2]}"
    local DEST="${ARGS[-1]}"
    local TEMP=_my-cloned--"$DEST"

    [ ! -e "$TEMP" ]
    [ -d "$DEST" ]

    std mkdir "$TEMP"  # Empty as required by `git clone`.
    git clone "${OPTS[@]}" --no-checkout -- "$REPO" "$TEMP"
    std mv "$TEMP"/.git "$DEST"/
    std rmdir "$TEMP"
    git -C "$DEST" checkout
}


# Variables

function _assign_MY_PLATFORM_IDS {
    declare -g MY_PLATFORM_IDS=()  # No duplicates, and no empties.
    local id x
    # (Maintenance: Keep in sync with _my_platform_id in profile.sh.)
    for id in                                                           \
        "${MY_PLATFORM_OS-}"         "${MY_PLATFORM_OS_ARCH-}"          \
        "${MY_PLATFORM_OS_VARIANT-}" "${MY_PLATFORM_OS_VARIANT_ARCH-}"  \
        "${MY_PLATFORM_OS_VAR_VER-}" "${MY_PLATFORM_OS_VAR_VER_ARCH-}"
    do
        if [ "$id" ]; then
            for x in "${MY_PLATFORM_IDS[@]}"; do
                if [ "$id" = "$x" ]; then
                    continue 2
                fi
            done
            MY_PLATFORM_IDS+=("$id")
        fi
    done
    readonly MY_PLATFORM_IDS
}
_assign_MY_PLATFORM_IDS
unset -f _assign_MY_PLATFORM_IDS


# Any source'ing of sub files must be done below here, so that the above are all defined for such.
