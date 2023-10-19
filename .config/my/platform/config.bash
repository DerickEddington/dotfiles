# Aspects that a user might want to customize for all platforms that I provision.

# shellcheck disable=SC2034  # These variables are used by what `source`s this file.


# Packages that I want installed in any of the platforms.  Each name must only be a single "word"
# according to Shell syntax.  These names follow my own convention (so that they're portable, and
# they're mapped to whatever is specific to each platform).  These might be installed by the OS
# distro's package manager system-wide, or these might be installed by some other means (e.g. PPAs
# system-wide, or pre-built user-local, or by building in each host user-local).  If a package is
# not available for a platform, or fails to install in a host, then a warning will be printed when
# the provisioning is run.
#
MY_PLATFORM_PACKAGES_DESIRED=(
    bash-completion
    command-not-found
    emacs-nox
    fd-find
    flock  # Of util-linux. Needed by _my_lock_combined_histfile.
    git  # Always useful for each user's ~/.dotfiles repository, at least.
    gnu-coreutils gnu-grep gnu-sed gnu-tar
    gnupg
    htop
    lsof
    most
    my-bash-history-combiner  # Needed by _my_histfile_combining.
    nano
    psmisc
    ripgrep
    screen
)

case "${MY_PLATFORM_OS_VARIANT}" in
    (Linux/Alpine)  MY_PLATFORM_PACKAGES_DESIRED+=(man-db man-pages tzdata) ;;
    (FreeBSD)       MY_PLATFORM_PACKAGES_DESIRED+=(groff pkg-provides terminfo-db) ;;
esac

# Change any of these to `yes`, as desired.
#
declare -A group=(
       [devel/C]=no
    [devel/Rust]=no
)

develCommon=(
    gnu-diffutils gnu-patch
    shellcheck bash-language-server
)

develCompiledCommon=(
    build-essential
    corrector-of-llvm-xray-stack-flame
    gdb
    gnu-autoconf gnu-binutils gnu-make
    "${develCommon[@]}"
)

if [ "${group[devel/C]}" = yes ]; then
    MY_PLATFORM_PACKAGES_DESIRED+=(
        bear
        clang clangd
        gcc
        "${develCompiledCommon[@]}"
    )
fi

if [ "${group[devel/Rust]}" = yes ]; then
    MY_PLATFORM_PACKAGES_DESIRED+=(
        cargo rust rust-analyzer
        "${develCompiledCommon[@]}"
    )
    case "${MY_PLATFORM_OS_VARIANT}" in
        (Linux/Alpine)  MY_PLATFORM_PACKAGES_DESIRED+=(rust-gdb) ;;
    esac
fi

unset group develCommon develCompiledCommon

readonly MY_PLATFORM_PACKAGES_DESIRED


# Where my external personal packages are installed from.
#
MY_PERSONAL_GIT_REPOSITORY=https://github.com/DerickEddington
