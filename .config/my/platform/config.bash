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
readonly MY_PLATFORM_PACKAGES_DESIRED=(
    bash-completion
   #clangd
    fd-find
    git
    gnu-coreutils
    htop
    most
    my-bash-history-combiner  # Needed by _my_histfile_combining.
    nano
    ripgrep
    screen
    util-linux  # Needed by _my_lock_combined_histfile to have `flock`.
    # TODO: more by default in main branch for all users
   #gcc
    # TODO: more by commented-out possibilities in main branch for all users to choose to uncomment.
)


# Where my external personal packages are installed from.
#
MY_PERSONAL_GIT_REPOSITORY=https://github.com/DerickEddington