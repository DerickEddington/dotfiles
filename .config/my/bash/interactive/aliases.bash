# enable color support with aliases
if [ -n "$LS_COLORS" ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)"
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

alias ls='ls -CFhv --group-directories-first --quoting-style=c-maybe --color=tty'
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
alias ps="ps fxu"
alias df="df -h"
alias free='free -h'
alias objdump='objdump -M intel-mnemonic'
alias emacs='emacs --no-window-system'
alias root='sudo su -l'
alias sul='sudo su -l'
alias sshfs='sshfs -o reconnect -o workaround=all -o idmap=user -o follow_symlinks -o cache=yes -o cache_timeout=30 -o kernel_cache -o large_read'
alias unsshfs='fusermount -u'
