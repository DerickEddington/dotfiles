# For bootstrapping my setup independently of my other more-involved package-installing modules.
#
_my_install_bash()              { sudo pkg install --no-refresh bash || [ $? -eq 4 ] ;}
_my_install_git()               { sudo pkg install --no-refresh git  || [ $? -eq 4 ] ;}

gnu() {
    # shellcheck disable=SC2145  # I know how `some"$@"` behaves and it's correct.

    if [ -x /usr/gnu/bin/"${1:?}" ]; then
        /usr/gnu/bin/"$@"
    else
        case "$1" in
            (sha*sum) /usr/bin/"$@" ;;
            (*)
                error "No GNU utility found for $(quote "$1")!"
                return 2
                ;;
        esac
    fi
}

_my_terminal_supports_colors() {
    if gnu tput setaf 1 > /dev/null 2>&1 ; then
        true
    else
        case "${TERM:-}" in
            (*color*) return 0 ;;
            (*)       return 1 ;;
        esac
    fi
}

_my_terminal_width() {
    # Use the ncurses `tput`, which is located under /usr/gnu/, because it uses ncurses' better
    # support for a larger variety of $TERM types.  This enables supporting, e.g.,
    # `screen.xterm-256color` which Solaris' own default `/usr/bin/tput` does not.
    gnu tput cols
}

_my_flock() {
    "$(_my_platspec_install_dir)"/bin/flock "$@"  # The `flock` of util-linux.
}
