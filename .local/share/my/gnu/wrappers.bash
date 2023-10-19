# Dynamically generate functions for all ./platform/$MY_PLATFORM_OS_VARIANT/bin/*
# Functions are used, instead of aliases, because having functions still allows elsewhere defining
# aliases with the same names - such aliases will use these functions.


function _my_gnu_generate_wrappers
{
    local - ; set -o nounset
    local myselfDir platformID platformDir gnuUtilExec

    myselfDir=$(std dirname "${BASH_SOURCE[0]}") || return
    myselfDir=$(abs_path "$myselfDir") || return

    for platformID in "${MY_PLATFORM_IDS[@]}"
    do
        platformDir=$myselfDir/platform/$platformID

        if [ -d "$platformDir"/bin ]
        then
            for gnuUtilExec in "$platformDir"/bin/*
            do
                local gnuUtilName
                gnuUtilName=$(std basename "$gnuUtilExec") || return

                if [ "$(type -t "$gnuUtilName")" != builtin ]
                then
                    # Sanity check that each name is a single word.
                    if ! is-single-word "$gnuUtilName"; then
                        error "GNU-utility name ${gnuUtilName@Q} is not a single word!"
                        return 2
                    fi

                    # Sanity check that each executable pathname is absolute.
                    if ! [[ "$gnuUtilExec" = /* ]]; then
                        error "GNU-utility executable pathname is not absolute!"
                        return 3
                    fi

                    if (( ${VERBOSE:-0} >= 2 )); then
                        if is-function-def "$gnuUtilName"; then
                            warn "Redefining function ${gnuUtilName@Q}."
                        fi
                    fi

                    eval "function $gnuUtilName { ${gnuUtilExec@Q} \"\$@\" ;}"
                else
                    : # Do not override Bash's builtins (that'd break things).
                fi
            done
        fi
    done
}

_my_gnu_generate_wrappers

unset _my_gnu_generate_wrappers
