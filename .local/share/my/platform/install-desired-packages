#! /usr/bin/env bash
set -o errexit -o nounset
(( "${VERBOSE:=0}" >= 3 )) && set -o xtrace
(( "${VERBOSE:=0}" >= 4 )) && set -o verbose


# shellcheck source=../bash/helpers.bash
source "${XDG_DATA_HOME:-$HOME/.local/share}"/my/bash/helpers.bash


self=$(std basename "$0"); readonly self

readonly config=$MY_CONFIG_HOME/my/platform/config.bash
readonly facility=$MY_DATA_HOME/my/platform/packages.bash

if [ -e "$config" ]; then
    ( # Follow user's choice of which packages to install, by loading and using the modules that
      # provide this facility.

      # shellcheck source=../../../../.config/my/platform/config.bash
      source "$config"  # Provides the below variable.
      # shellcheck source=./packages.bash
      source "$facility"  # Provides the below command.

      my-platform-install-packages "${MY_PLATFORM_PACKAGES_DESIRED[@]}"
    )
else
    warn "$self: Missing ${config@Q}. Doing nothing."
fi
