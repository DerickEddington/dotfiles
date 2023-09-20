unalias -a  # Remove all alias definitions done by previous other sections of init.

# enable color support with aliases
if [ -n "$LS_COLORS" ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)"
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
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
alias pstree="pstree -a -l -n"
alias nano="nano -w"
alias df="df -h"
alias objdump='objdump -M intel-mnemonic'
alias emacs='emacs --no-window-system'
alias root='sudo su -l'
alias sul='sudo su -l'
alias sshfs='sshfs -o reconnect -o workaround=all -o idmap=user -o follow_symlinks -o cache=yes -o cache_timeout=30 -o kernel_cache -o large_read'
alias unsshfs='fusermount -u'
