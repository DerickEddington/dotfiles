# General helpers for both scripted and interactive shell use.
# Source'ing this must be idempotent, in case it's ever source'd multiple times.

# shellcheck disable=SC2034  # These variables are used by the things that source this.


# Avoid source'ing this file more than once.

if ! declare -g -p -F _my_bash_sourced_already >& /dev/null  # Not defined yet.
then
    function _my_bash_sourced_already
    {
        local - ; set -o nounset -o errexit
        declare -g -A _MY_BASH_SOURCED_ALREADY

        if [[ -v _MY_BASH_SOURCED_ALREADY["$1"] ]]; then
            return 0  # Enables `_my_bash_sourced_already ... && return` to return 0.
        else
            # Set immediately, to avoid the possibility of recursive source'ing.
            _MY_BASH_SOURCED_ALREADY["$1"]=true
        fi
        return 1
    }
    declare -g -f -r _my_bash_sourced_already || return  # Make it readonly.
fi

_my_bash_sourced_already local/share/my/bash/helpers && return


# Also provide all of these.
source "$(command -p dirname "${BASH_SOURCE[0]}")"/../sh/helpers.sh  # (Must be outside any function.)


# For defining functions.  It's assumed that it's OK to redefine functions that didn't use these.

! declare -g -p -F is-function-def >& /dev/null || return  # Must not be defined yet.
function is-function-def {
    local - ; set -o nounset -o errexit
    declare -g -p -F -- "$1" >& /dev/null || return
}

! is-function-def is-function-undef || return
function is-function-undef {
    ! is-function-def "$@"
}

is-function-undef declare-function-readonly-lax || return
function declare-function-readonly-lax {
    local - ; set -o nounset -o errexit
    declare -g -f -r -- "$1" || return
}

is-function-undef declare-function-readonly || return
function declare-function-readonly {
    local - ; set -o nounset -o errexit
    declare-function-readonly-lax "$@"  # Exit on error.
}

declare-function-readonly is-function-def
declare-function-readonly is-function-undef
declare-function-readonly declare-function-readonly-lax
declare-function-readonly declare-function-readonly


# For robustness

# Already defined in ../sh/helpers.sh.
declare-function-readonly std

is-function-undef is-command-extant || return
function is-command-extant {
    command -v "${1:-}" > /dev/null 1>&2
}
declare-function-readonly is-command-extant

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


# Miscellaneous

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


# Any source'ing of sub files must be done below here, so that the above are all defined for such.
