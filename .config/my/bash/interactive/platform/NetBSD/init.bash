# Enable Bash completion.
#
if [ -f /usr/pkg/share/bash-completion/bash_completion ]
then
    if [ "${MY_CONFIG_HOME-}" ] && [ "${MY_PLATFORM_OS-}" ]; then
        # shellcheck disable=SC2034  # This variable is used by what's `source`d next.
        BASH_COMPLETION_USER_DIR=$MY_CONFIG_HOME/my/bash/interactive/platform/$MY_PLATFORM_OS
    fi
    source /usr/pkg/share/bash-completion/bash_completion
fi

# Use the man pages for the GNU utils, because those utils are my primary (via my wrapper funcs).
# (Don't use MANPATH because that overrides (replaces) the system's defaults but we still want
# those too.) (NetBSD's man-page for `man` itself says that the `-m` dir(s) "will be searched
# before the standard directories" but in NetBSD 9 that seems to be untrue and they're searched
# after, which is annoying but at least `man -a` can be used to see them still.  It seems like
# NetBSD 10 fixed `-m` to behave as documented.)
#
function man { std man -m /usr/pkg/gnu/man "$@" ;}
