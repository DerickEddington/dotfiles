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

    ( shopt -s assoc_expand_once  # Critical for correctness of `perMethods` keys! Insane!
      local -A perMethods

      for name in "${names[@]}"; do
          # Sanity check that each name is a single word.
          if ! is-single-word "$name"; then
              error "Package name ${name@Q} is not a single word!"
              return 2
          fi

          # Get the platform-specific name.
          if [[ -v MY_PLATFORM_SPECIFIC_PACKAGES_NAMES["$name"] ]]; then
              local specName=${MY_PLATFORM_SPECIFIC_PACKAGES_NAMES["$name"]}
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
          if [[ -v MY_PLATFORM_SPECIFIC_PACKAGES_METHODS["$specName"] ]]; then
              local method=${MY_PLATFORM_SPECIFIC_PACKAGES_METHODS["$specName"]}
              # Accumulate per-method list of packages.  This is why each name must be a single
              # word, so a list can simply be a single string with elements separated by
              # whitespace.
              perMethods["$method"]+="$specName "
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
          local specNamesList=${perMethods["$method"]} specNamesArray
          split-on-words "$specNamesList" specNamesArray

          eval "$method "'"${specNamesArray[@]}"' || {
              warn "Package(s) (${specNamesArray[*]@Q}) failed to install. Ignoring."
              continue
          }
      done
    )
}


function my-cargo-install-user-local
{
    local - ; set -o nounset
    (( $# >= 1 )) || return
    local -r crate="$1" opts=("${@:2}")
    local via=()

    if ! is-command-extant cargo ; then
        my-platform-install-packages rust cargo || return  # Ensure they're installed.
    fi

    case "$crate" in
        (http*://*) via=(--git) ;;
        (file:*)    via=(--path) ; crate=${crate#file:} ;;  # TODO: Keep the "file:" prefix?
        (*:*)       error "Unsupported URI scheme!" ; return 2 ;;
        (*)         via=() ;;  # From a registry (usually Crates.io).
    esac

    # Install executables into ~/.local/bin/ (the XDG-BDS dir that should already be in PATH).
    #
    cargo install --root ~/.local "${opts[@]}" "${via[@]}" "$crate" || return
}

function my-cargo-install-user-local-from-my-repo
{
    local - ; set -o nounset
    (( $# >= 1 )) || return
    local -r specName="$1" opts=("${@:2}")

    if ! [ "${MY_PERSONAL_GIT_REPOSITORY:-}" ]; then
        error "MY_PERSONAL_GIT_REPOSITORY not defined!"
        return 2
    fi
    local -r crate=$MY_PERSONAL_GIT_REPOSITORY/$(printf %s.git "$specName")

    my-cargo-install-user-local "$crate" "${opts[@]}"
}
