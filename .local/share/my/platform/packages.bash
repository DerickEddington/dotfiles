# Provides the generic procedures for using each platform's specification of packages to install.

source "$(command -p  dirname "${BASH_SOURCE[0]}")"/../bash/helpers.bash


# If already source'd, don't do anything.
_my_bash_sourced_already local/share/my/platform/packages && return


# shellcheck source=./Linux/Debian/packages.bash  #  (Just one of many, to have something.)
source "$(std dirname "${BASH_SOURCE[0]}")"/"$MY_PLATFORM_OS_VARIANT"/packages.bash


function my-platform-install-packages
{
    local - ; set -o nounset
    local names=("$@")
    remove-dups names  # Not required, but why not.
    readonly names
    local name method errPlatform=${MY_PLATFORM_OS_VARIANT:-unknown}

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

      # Apply each method to the names that are associated with it.
      #
      for method in "${!perMethods[@]}"; do
          local specNamesList=${perMethods["$method"]} specNames specName cmds=() cmd
          split-on-words "$specNamesList" specNames
          remove-dups specNames  # Important to prevent multiple invocations of the same.

          local methodCmd rc=0
          methodCmd=$(unprefix-cmd single "$method") || rc=$?
          case $rc in
              (0) # Was prefixed.
                  # Apply the method to each of its names separately.  This enables passing
                  # options that are unique to each name, and this also helps avoid ambiguity in
                  # methods' argument handling.
                  #
                  for specName in "${specNames[@]}"; do
                      cmds+=("$methodCmd ${specName@Q}")
                  done
                  ;;
              (1) # Was not prefixed, but is valid.
                  # Apply the method to all of its names together at once.  This enables
                  # installing multiple packages at once per method, which can be more efficient
                  # (as opposed to doing a separate install command for each package, which can be
                  # slower (e.g. with Solaris' `pkg`)).
                  # shellcheck disable=SC2016  # Want these single-quoted expressions.
                  #
                  cmds=("$methodCmd "'"${specNames[@]}"')
                  ;;
              (2) # Invalid.
                  error "Method ${method@Q} is not valid syntax!"
                  return 5
                  ;;
          esac

          for cmd in "${cmds[@]}"
          do
              # Method commands are `eval`ed because this enables them to use Shell syntax
              # (e.g. for quoting their options).
              #
              eval "$cmd" || {
                  cmd=$(eval "print $cmd")  # To show the expanded command.
                  warn "Package installation failed: ${cmd@Q}. Ignoring."
              }
          done
      done
    )
}


function my-cargo-install-user-local
{
    local - ; set -o nounset
    (( $# >= 1 )) || return
    local -r args=("$@")
    local -r crate="${args[-1]}" opts=("${args[@]:0:${#args[@]}-1}")
    local via=()
    # (Maintenance: Keep in sync with the pathname scheme that ~/.config/my/env/profile.sh uses.)
    local -r platspecDir=my/platform/${MY_PLATFORM_OS_VAR_VER_ARCH:?}
    local -r installDir=${HOME:?}/.local/$platspecDir/installed

    if ! is-command-found cargo ; then
        # Ensure Rust & Cargo are installed.
        my-platform-install-packages rust cargo  # (Ignores if couldn't install.)
        if ! is-command-found cargo ; then
            if my-install-rustup "$installDir" ; then
                if ! is-command-found cargo ; then
                    prepend_to_PATH_if_ok "$installDir"/bin
                fi
            else
                warn "Failed to install RustUp (as fallback)!"
            fi
        fi
        if ! is-command-found cargo ; then
            warn "\`cargo\` still not in PATH!"
        fi
    fi

    case "$crate" in
        (http*://*) via=(--git) ;;
        (file:*)    via=(--path) ; crate=${crate#file:} ;;  # TODO: Keep the "file:" prefix?
        (*:*)       error "Unsupported URI scheme!" ; return 2 ;;
        (*)         via=() ;;  # From a registry (usually Crates.io).
    esac

    # Install binaries into the architecture-specific & platform-specific location.
    # ~/.config/my/env/profile.sh should configure that's bin/ and lib/ sub-dirs to be in the
    # PATH and LD_LIBRARY_PATH.

    cargo install --root "$installDir" "${opts[@]}" "${via[@]}" "$crate"
}

function my-cargo-install-user-local-from-my-repo
{
    local - ; set -o nounset
    (( $# >= 1 )) || return
    local -r args=("$@")
    local -r specName="${args[-1]}" opts=("${args[@]:0:${#args[@]}-1}")

    if ! [ "${MY_PERSONAL_GIT_REPOSITORY:-}" ]; then
        error "MY_PERSONAL_GIT_REPOSITORY not defined!"
        return 2
    fi
    local -r crate=$MY_PERSONAL_GIT_REPOSITORY/$(printf "%s.git" "$specName")

    my-cargo-install-user-local "${opts[@]}" "$crate"
}

function my-install-rustup
{
    # The advantage of installing Rust & Cargo this way is that it doesn't require superuser
    # privilege.  (That is why this doesn't try to install a host system's `rustup` package.)

    if [ "${1-}" ]; then
        local -r installDir=$1
    else
        # (Maintenance: Keep in sync with the scheme that ~/.config/my/env/profile.sh uses.)
        local -r platspecDir=my/platform/${MY_PLATFORM_OS_VAR_VER_ARCH:?}
        local -r installDir=${HOME:?}/.local/$platspecDir/installed
    fi
    # You may provide this as an environment variable.  E.g. a pre-downloaded `rustup-init`
    # executable, which can be useful when without network.  Or e.g. some other version of the
    # Shell script that downloads `rustup-init` like `sh.rustup.rs` does.
    local installer=${MY_RUSTUP_INSTALLER-}
    local -r installArgs=(-y --default-toolchain stable --profile minimal --no-modify-path)
    local -r rustupURL=https://sh.rustup.rs
    local invoke=()

    if ! [ "$installer" ]; then
        installer=${MY_RUNTIME_DIR:?}/my/rustup.sh
        std mkdir -p "$(std dirname "$installer")"
        _my-download-https "$rustupURL" > "$installer" || {
            error "Failed to download RustUp!"
            return 1
        }
        std chmod a-x "$installer"
    fi
    if ! [ -x "$installer" ]; then invoke=(std sh); fi

    # CARGO_HOME is just to capture its bin/* next.
    # CARGO_INSTALL_ROOT is pointless as of 2023-10-03, but for the future?
    CARGO_HOME=$installDir/cargo        \
    CARGO_INSTALL_ROOT=$installDir      \
        "${invoke[@]}" "$installer" "${installArgs[@]}" || return

    # Place RustUp's proxy executables according to my scheme (which will be automatically added
    # to PATH), because these are OS-specific, sometimes platform-variant-specific, and
    # architecture-specific.  But CARGO_HOME and RUSTUP_HOME are not specific, so let them default
    # to ~/.cargo/ and ~/.rustup/ so they can be shared when $HOME is used across multiple hosts.
    #
    std mkdir -p "$installDir"/bin || return
    std mv "$installDir"/cargo/bin/* "$installDir"/bin/ || return
    std rm -r -f "$installDir"/cargo

    if ! [ -L ~/.rustup/toolchains ] \
    && ! std grep -q -s -E -e '^/.rustup/toolchains/' ~/.gitignore
    then
        println $'# Added by my-install-rustup\n/.rustup/toolchains/' >> ~/.gitignore
    fi
}
