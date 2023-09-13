# TODO: describe


source "$(dirname "${BASH_SOURCE[0]}")"/../bash/helpers.bash


# If already source'd, don't do anything.
_my_bash_sourced_already local/share/my/platform/packages && return


# shellcheck source=./Linux/Ubuntu/22.04/packages.bash  #  (Just one of many, to have something.)
source "$(dirname "${BASH_SOURCE[0]}")"/"$MY_PLATFORM"/packages.bash


function my-platform-install-packages
{
    local - ; set -o nounset
    local -r names=("$@")
    local name method errPlatform=${MY_PLATFORM:-Unknown}

    if ! is-var-assoc-array MY_PLATFORM_SPECIFIC_PACKAGES_NAMES \
    || ! is-var-assoc-array MY_PLATFORM_SPECIFIC_PACKAGES_METHODS
    then
        error "${FUNCNAME[0]}: Needed variables are not set or not valid type!"
        return 1
    fi

    for name in "${names[@]}"; do
        # Sanity check that each name is a single word.
        split-on-words "$name"
        if (( ${#WORDS_SPLITTED[@]} >= 2 )); then
            error "Package name ${name@Q} is not a single word!"
            return 2
        fi

        # Get the platform-specific name.
        if [[ -v MY_PLATFORM_SPECIFIC_PACKAGES_NAMES[$name] ]]; then
            local specName=${MY_PLATFORM_SPECIFIC_PACKAGES_NAMES[$name]}
            # If it's the empty string, this means to ignore it.
            if [ "$specName" = "" ]; then
                warn "Package ${name@Q} is not available for $errPlatform. Ignoring it."
                continue
            fi
        else
            error "Package ${name@Q} is not defined for $errPlatform!"
            return 3
        fi

        # Get the platform-specific method to install the named.
        if [[ -v MY_PLATFORM_SPECIFIC_PACKAGES_METHODS[$specName] ]]; then
            local method=${MY_PLATFORM_SPECIFIC_PACKAGES_METHODS[$specName]}
            # Accumulate per-method list of packages.  This is why each name must be a single
            # word, so a list can simply be a single string with elements separated by whitespace.
            local -A perMethods[$method]+="$specName "
        else
            error "Platform-specific package name ${specName@Q} does not have a method!"
            return 4
        fi
    done

    # The names are all processed above, to detect any issues, before installing any of them.

    # Do each method with all the names that are for a method.  This enables installing multiple
    # packages at once per method, which can be more efficient (as opposed to doing a separate
    # install command for each package, which can be slower (e.g. with Solaris' `pkg`)).
    for method in "${!perMethods[@]}"; do
        local perMethodNames ; split-on-words "${perMethods[$method]}" perMethodNames
        $method "${perMethodNames[@]}" || {
            warn "Package ${name@Q} (${specName@Q}) failed to install. Ignoring it."
            continue
        }
    done
}
