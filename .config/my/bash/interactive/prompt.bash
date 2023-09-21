_my_bash_prompt_sep_prefix="------------"
_my_bash_prompt_sep_with_time="----------------------------------------"
_my_bash_prompt_sep_with_time+=" \D{%F %T %Z} ---"

function _my_bash_prompt_setup()
{
    local DO_COLORS="$1"

    if [ "$DO_COLORS" = yes ]; then
        # Note: PS0 does not like having \[ \].
        local GREY="\e[00;37m"
        local NO_COLOR="\e[00m"
    else
        local GREY=""
        local NO_COLOR=""
    fi

    # Preserve any preexisting PROMPT_COMMAND and execute it.  NixOS /etc/bashrc
    # sets it for showing info in VTE terminals' window title.  The preexisting
    # command must be executed after, so that _my_bash_prompt_command is
    # executed first, as needed to capture the PREV_EXIT_STATUS=$? of the user's
    # command.
    PROMPT_COMMAND="_my_bash_prompt_command $DO_COLORS ${PROMPT_COMMAND:+;} ${PROMPT_COMMAND:-}"

    PS0="${GREY}${_my_bash_prompt_sep_prefix}"
    PS0+="${_my_bash_prompt_sep_with_time}${NO_COLOR}\n"
}

function _my_bash_prompt_command()
{
    local PREV_EXIT_STATUS=$?
    local - ; set +o xtrace +o verbose  # When user has these enabled, don't want for prompt.
    local DO_COLORS="$1"

    # shellcheck disable=SC2034  # Unused variables left for readability.
    if [ "$DO_COLORS" = yes ]; then
        local RED="\[\e[00;31m\]"
        local GREEN="\[\e[00;32m\]"
        local YELLOW="\[\e[00;33m\]"
        local BLUE="\[\e[00;34m\]"
        local WHITE="\[\e[01;37m\]"
        local GREY="\[\e[00;37m\]"
        local MAGENTA="\[\e[00;35m\]"
        local NO_COLOR="\[\e[00m\]"
    else
        local RED=""
        local GREEN=""
        local YELLOW=""
        local BLUE=""
        local WHITE=""
        local GREY=""
        local MAGENTA=""
        local NO_COLOR=""
    fi

    if ((PREV_EXIT_STATUS == 0)); then
        local STATUS_PREFIX="${GREY}${_my_bash_prompt_sep_prefix}"
    else
        local PAD=""
        local WIDTH=${#PREV_EXIT_STATUS}
        while ((WIDTH++ < 3)); do
            PAD+="-"
        done
        local STATUS_PREFIX="${RED}!--- \\\$?=${PREV_EXIT_STATUS} ${PAD}"
    fi

    if [ $EUID -eq 0 ]; then
        local USER_HOST_COLOR="${MAGENTA}"
        local CWD_COLOR="${BLUE}"
        local SIGIL_COLOR="${RED}"
    else
        local USER_HOST_COLOR="${GREEN}"
        local CWD_COLOR="${BLUE}"
        local SIGIL_COLOR="${YELLOW}"
    fi
    PS1="${STATUS_PREFIX}${_my_bash_prompt_sep_with_time}\n"
    PS1+="${USER_HOST_COLOR}\u@\h ${CWD_COLOR}\w\n"
    PS1+="${SIGIL_COLOR}\\\$${NO_COLOR} "

    # TODO: Is this still good on NixOS?  It does something similar in
    # /etc/bashrc, also in addition to what its /etc/bashrc does for VTE
    # terminals' titles.
    # Set the title to: user@host dir
    case "$TERM" in
        xterm*|rxvt*|screen|vt100)
            PS1="\[\e]0;\u@\h: \w\a\]$PS1"
            ;;
        *)
            ;;
    esac
}

# NixOS
[ -n "$PROMPT_COLOR" ] && color_prompt=yes

_my_bash_prompt_setup "$color_prompt"

# unset -f _my_bash_prompt_setup
