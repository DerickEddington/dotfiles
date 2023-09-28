unalias -a  # Remove all alias definitions done by previous other sections of init.


# This only defines an alias if its backing command is found.  This helps avoid the problem where,
# once an alias has been defined, `is_command_found` will return true just because there is an
# alias even though the backing command is still not found, which confuses my logic that uses
# `is_command_found`.
#
function _def_alias_if_backed {
    local -r aliasName=${1:?} aliasCmd=${2:?} aliasArgs=("${@:3}")
    if is_command_found "$aliasCmd" ; then
        # shellcheck disable=SC2139
        alias "$aliasName"="$aliasCmd ${aliasArgs[*]}"
    fi
}


# enable color support with aliases
if [ -n "$LS_COLORS" ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)"
    _def_alias_if_backed dir dir --color=auto
    _def_alias_if_backed vdir vdir --color=auto
    _def_alias_if_backed grep grep --color=auto
    _def_alias_if_backed fgrep fgrep --color=auto
    _def_alias_if_backed egrep egrep --color=auto
fi

_my_gnu_ls_alias='ls -CFhv --group-directories-first --quoting-style=c-maybe --color=tty'

case "$MY_PLATFORM_OS"
in
    (Linux)
        # shellcheck disable=SC2139  # Want this to be expanded when defined.
        alias ls="$_my_gnu_ls_alias"
        alias free='free -h'
        alias ps="ps fxu"
        ;;
    (FreeBSD)
        if [ -x /usr/local/bin/gls ]; then  # GNU ls
            # Use the "g"-prefixed name, instead of assuming that `ls` is the wrapper from
            # my/gnu/wrappers.bash, in case that wrapper is missing/broken, so that this critical
            # alias is more robust.
            # shellcheck disable=SC2139  # Want this to be expanded when defined.
            alias ls=g"$_my_gnu_ls_alias"
        else
            alias ls='ls -CFhG'
        fi
        alias ps="ps dxu"
        ;;
esac
unset _my_gnu_ls_alias

alias p='println'
alias l='ls -A -l'
alias ll='ls -l'
alias la='ls -A'
alias lla='ll -A'
alias lt='lla -t'
alias lr='lla -R'
alias ltr='lt -R'
alias lrt='lr -t'
alias rm="rm -i -r"
alias rmdir="rm -i -r"
_def_alias_if_backed pstree pstree -a -l -n
_def_alias_if_backed nano nano -w
_def_alias_if_backed df df -h
_def_alias_if_backed objdump objdump -M intel-mnemonic
_def_alias_if_backed emacs emacs --no-window-system
alias root='sudo su -l'
alias sul='sudo su -l'
_def_alias_if_backed sshfs sshfs -o reconnect -o workaround=all -o idmap=user -o follow_symlinks \
                     -o cache=yes -o cache_timeout=30 -o kernel_cache -o large_read
_def_alias_if_backed unsshfs fusermount -u
