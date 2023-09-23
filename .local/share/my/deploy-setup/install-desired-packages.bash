#! /usr/bin/env bash
# shellcheck source=../bash/helpers.bash
source "${XDG_DATA_HOME:-$HOME/.local/share}"/my/bash/helpers.bash
_my-script-prelude


readonly config=$MY_CONFIG_HOME/my/platform/config.bash
readonly facility=$MY_DATA_HOME/my/platform/packages.bash

if [ -e "$config" ]; then
    # Follow user's choice of which packages to install, by loading and using the modules that
    # provide this facility.

    # shellcheck source=../../../../.config/my/platform/config.bash
    source "$config"  # Provides the below variable.
    # shellcheck source=../platform/packages.bash
    source "$facility"  # Provides the below command.

    my-platform-install-packages "${MY_PLATFORM_PACKAGES_DESIRED[@]}"
else
    warn "$selfBase: Missing ${config@Q}. Doing nothing."
fi
