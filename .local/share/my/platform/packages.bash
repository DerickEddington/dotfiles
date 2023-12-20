# Provides the generic procedures for using each platform's specification of packages to install.

source "$(command -p  dirname "${BASH_SOURCE[0]}")"/../bash/helpers.bash


# If already source'd, don't do anything.
_my_bash_sourced_already local/share/my/platform/packages && return


# These can sometimes be useful as the values of MY_PLATFORM_SPECIFIC_PACKAGES_NAMES and as the
# keys of MY_PLATFORM_SPECIFIC_PACKAGES_METHODS.
# shellcheck disable=SC2034
#
declare -A github=(
    [fd-find]=https://github.com/sharkdp/fd.git
    [ripgrep]=https://github.com/BurntSushi/ripgrep.git
)

declare -A jedsoft=(
    [most]=https://www.jedsoft.org/releases/most/most-5.2.0.tar.gz
    [most-5.2.0.tar.gz.sha1sum]=322073ee6e8c45ce084f4fccd08d3f026aa1f66d
)


# shellcheck source=./SunOS/OpenIndiana/packages.bash  #  (Just one of many, to have something.)
source "$(std dirname "${BASH_SOURCE[0]}")"/"${MY_PLATFORM_OS_VARIANT:?}"/packages.bash


function my-platform-install-packages
{
    local - ; set -o nounset
    local names=("$@")
    remove-dups names  # Not required, but why not.
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


function my-npm-install {
    if ! is-command-found npm ; then my-platform-install-packages npm ; fi
    sudo npm install --global --omit=dev "$@"
}


function my-cargo-install-single
{
    local - ; set -o nounset
    (( $# >= 1 )) || return
    local args=("$@")
    local crate="${args[-1]}" opts=("${args[@]:0:${#args[@]}-1}")
    local via=()
    local installDir="$(_my_platspec_install_dir)"

    if ! is-command-found cargo ; then
        # Ensure Rust & Cargo are installed.
        my-platform-install-packages rust cargo || return  # (Ignores if couldn't install.)
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
    #
    # (Would've given `profile.release.strip=true` but that didn't work in OpenIndiana with its
    # Cargo 1.70.0.  It encountered a bug (when building `ripgrep`) that caused an error like:
    # `ld: fatal: symbol '__rustc_debug_gdb_scripts_section__' in file ....rcgu.o associated with
    # invalid section`.)

    cargo install --root "$installDir" --locked --config profile.release.strip=\"debuginfo\" \
                  "${opts[@]}" "${via[@]}" "$crate"
}

function _my-cargo-install-multiple
{
    # shellcheck disable=SC2206
    local opts=(${opts-})  # From env var. Want word-splitting (and globbing, I guess).
    local installSingle=${1:?}
    local crate
    for crate in "${@:2}" ; do
        "$installSingle" "${opts[@]}" "$crate"
    done
}

function my-cargo-install {
    _my-cargo-install-multiple my-cargo-install-single "$@"
}

function my-cargo-install--fd-find
{
    local features=() patches=()
    local noJEmalloc=(--no-default-features --features completions)  # No `use-jemalloc`.
    local patchUsersDep=(  # Needed for support for illumos, and maintained.
        --config
        patch.crates-io.users.git=\""${MY_PERSONAL_GIT_REPOSITORY:?}"/uzers-as-users.git\"  # TOML
    )
    if [ "${MY_PLATFORM_OS_VARIANT:?}" = SunOS/OpenIndiana ]; then
        features+=("${noJEmalloc[@]}")
        patches+=("${patchUsersDep[@]}")
    fi
    my-cargo-install-single "${features[@]}" "${patches[@]}" "$@"
}

function my-cargo-install--ripgrep {
    my-platform-install-packages lib/pcre2 || return
    my-cargo-install-single --features pcre2 "$@"
}

function my-cargo-install-single-from-my-repo
{
    local - ; set -o nounset
    (( $# >= 1 )) || return
    local args=("$@")
    local specName="${args[-1]}" opts=("${args[@]:0:${#args[@]}-1}")

    if ! [ "${MY_PERSONAL_GIT_REPOSITORY:-}" ]; then
        error "MY_PERSONAL_GIT_REPOSITORY not defined!"
        return 2
    fi
    local crate=$MY_PERSONAL_GIT_REPOSITORY/$(printf "%s.git" "$specName")

    my-cargo-install-single "${opts[@]}" "$crate"
}

function my-cargo-install-from-my-repo {
    _my-cargo-install-multiple my-cargo-install-single-from-my-repo "$@"
}

function my-install-rustup
{
    if is-command-found rustup ; then
        # Ensure these are installed, since they might not be if
        # `my-rustup-install--rust-analyzer` ran first.
        rustup component add cargo rustc rust-std clippy rustfmt rust-docs
    else
        # The advantage of installing Rust & Cargo this way is that it doesn't require superuser
        # privilege.  (That is why this doesn't try to install a host system's `rustup` package.)

        if [ "${1-}" ]; then
            local installDir=$1
        else
            local installDir="$(_my_platspec_install_dir)"
        fi
        # You may provide this as an environment variable.  E.g. a pre-downloaded `rustup-init`
        # executable, which can be useful when without network.  Or e.g. some other version of the
        # Shell script that downloads `rustup-init` like `sh.rustup.rs` does.
        local installer=${MY_RUSTUP_INSTALLER-}
        local installArgs=(-y --default-toolchain stable --profile default --no-modify-path
                              "${@:2}")
        local rustupURL=https://sh.rustup.rs
        local invoke=()

        if ! [ "$installer" ]; then
            installer=${MY_RUNTIME_DIR:?}/my/rustup.sh
            std mkdir -p "$(std dirname "$installer")"
            _my-download-https "$rustupURL" > "$installer" || return
            std chmod a-x "$installer"
        fi
        if ! [ -x "$installer" ]; then invoke=(bash); fi  # Must use a shell that has `local`.

        # CARGO_HOME is just to capture its bin/* next.
        # CARGO_INSTALL_ROOT is pointless as of 2023-10-03, but for the future?
        CARGO_HOME=$installDir/cargo        \
        CARGO_INSTALL_ROOT=$installDir      \
            "${invoke[@]}" "$installer" "${installArgs[@]}" || return

        # Place RustUp's proxy executables according to my scheme (which will be automatically
        # added to PATH), because these are OS-specific, sometimes platform-variant-specific, and
        # architecture-specific.  But CARGO_HOME and RUSTUP_HOME are not specific, so let them
        # default to ~/.cargo/ and ~/.rustup/ so they can be shared when $HOME is used across
        # multiple hosts.
        #
        std mkdir -p "$installDir"/bin || return
        std mv "$installDir"/cargo/bin/* "$installDir"/bin/ || return
        std rm -r -f "$installDir"/cargo

        if ! [ -L ~/.rustup/toolchains ] \
        && ! std grep -q -s -E -e '^/.rustup/toolchains/' ~/.gitignore
        then
            println $'# Added by my-install-rustup\n/.rustup/toolchains/' >> ~/.gitignore
        fi
    fi
}

function my-rustup-install--rust-analyzer
{
    local name=${1:?}
    [ "$name" = rust-analyzer ] || exit

    if ! is-command-found "$name"
    then
        local installDir=$(_my_platspec_install_dir)

        if is-command-found rustup
        then
            rustup component add "$name"
        else
            # Installing `rustup` to have `rust-analyzer` will also install `cargo` etc. from
            # that, which will take precedence over any system-wide installation of Rust.  While
            # it's possible to uninstall those components of `rustup` and remove their proxy
            # executables, that would interfere with the functioning of that `rust-analyzer`.  I
            # usually prefer the latest version of Rust over older versions installed by a
            # system's package, anyway.  But this is not ideal for just having `rust-analyzer`,
            # and a better approach would be for Debian & Ubuntu to provide a package for it (like
            # FreeBSD and OpenIndiana do) which uses the same "sysroot" (and is of the same
            # version) as their package for Rust.
            #
            if ! my-install-rustup "$installDir" --component "$name"
            then
                error "Failed to install RustUp!"
                return 1
            fi
        fi
    fi
}


function my-deps-install
{
    local deps=()

    while (( $# >= 1 )) && [ "$1" != "--" ]; do
        deps+=("$1")
        shift
    done

    my-platform-install-packages "${deps[@]}" || return

    if [ "${1-}" = "--" ]; then
        local mainCmd=("${@:2}")
        "${mainCmd[@]}"
    fi
}


function my-jedsoft-build-and-install
{
    local name=${1:?}

    if ! is-command-found "$name"
    then
        local url=${jedsoft[$name]:?}
        local file=$(std basename "$url")
        local checksum=${jedsoft["$file".sha1sum]:?}
        local dir=${file%.tar.gz}
        local installDir=$(_my_platspec_install_dir)
        local workDir=$(gnu mktemp -d)

        my-platform-install-packages build-essential lib/slang/dev || return

        ( cd "$workDir" || exit

          _my-download-https "$url" > "$file" || exit
          gnu sha1sum -c <<<"$checksum *$file"$'\n' || exit

          gnu tar -x -z -f "$file" || exit
          cd "$dir" || exit
          { ./configure --prefix="$installDir" || exit ;} |& std tee configure.out
          { gnu make -j "$(gnu nproc)" install || exit ;} |& std tee make.out

        ) || return

        std rm -r -f "$workDir"
    fi
}


function my-install-files {
    gnu install -D --backup=numbered "$@"
}


function my-build-and-install--flock
{
    # This is a hack to build only the `flock` utility of the util-linux collection of Linux
    # utilities.  Even though much of util-linux can't be built, nor work, in Solaris, the `flock`
    # utility is simpler and OpenIndiana provides the same API for the underlying `flock` C
    # function.  FreeBSD also provides the `flock` utility of util-linux (in its "ports") based on
    # it also providing the same underlying API (I assume).  My very-basic testing of this build
    # of `flock` in OpenIndiana showed that it seems to be working.

    local name=${1:?}
    [ "$name" = flock ] || exit

    if ! is-command-found "$name"
    then
        local ver=2.39.2
        local dir=util-linux-$ver
        local file=$dir.tar.xz
        local mirror=https://mirrors.edge.kernel.org/pub/linux/utils
        local url=$mirror/util-linux/v${ver%.2}/$file
        local checksum=87abdfaa8e490f8be6dde976f7c80b9b5ff9f301e1b67e3899e1f05a59a1531f

        local thisFuncDefDir=$(std dirname "${BASH_SOURCE[0]}")
        local patch=$thisFuncDefDir/"${MY_PLATFORM_OS_VARIANT:?}"/flock.patch

        local installDir=$(_my_platspec_install_dir)
        local workDir=$(gnu mktemp -d)

        my-platform-install-packages build-essential || return

        ( cd "$workDir" || exit
          set -o pipefail

          _my-download-https "$url" > "$file" || exit
          gnu sha256sum -c <<<"$checksum *$file"$'\n' || exit
          gnu tar -x -J -f "$file" || exit

          if [ -e "$patch" ]; then
              std patch -p 0 < "$patch" || exit
          fi
          cd "$dir" || exit
          export PATH=/usr/gnu/bin:$PATH
          ./configure --prefix="$installDir" |& std tee configure.out || exit
          gnu make -j "$(gnu nproc)" flock |& std tee make.out || exit

          my-install-files --mode=u=rw,go=r ./sys-utils/flock.1 "$installDir"/share/man/man1/
          my-install-files --strip ./flock "$installDir"/bin/ || exit

        ) || return

        std rm -r -f "$workDir"
    fi
}


function my-build-and-install--clangd
{
    local name=${1:?}
    [ "$name" = clangd ] || exit

    if ! is-command-found "$name"
    then
        local ver=15.0.7  # The last version that works in OpenIndiana.
        local dir=llvm-project-$ver.src
        local file=$dir.tar.xz
        local site=https://github.com/llvm/llvm-project/releases/download
        local url=$site/llvmorg-$ver/$file
        local checksum=8b5fcb24b4128cf04df1b0b9410ce8b1a729cb3c544e6da885d234280dedeac6

        local installDir=$(_my_platspec_install_dir)
        local workDir=$(TMPDIR=/var/tmp  gnu mktemp -d)

        my-platform-install-packages build-essential cmake ninja || return

        # # Using Clang to build it enables statically linking to LLVM's libc++.  Might as well use
        # # the same Clang version.
        # local cc=clang cxxc=clang++
        # local comp
        # for comp in "$cc" "$cxxc" ; do
        #     if ! is-command-found "$comp" ; then
        #         error "Desired compiler ${comp@Q} not found in PATH!"
        #         return 2
        #     fi
        #     if ! [[ "$("$comp" --version | std head -n1)" =~ ^clang\ version\ $ver ]]; then
        #         error "Desired compiler version is not the same!"
        #         return 3
        #     fi
        # done
        # Along with:
        #     -DCMAKE_C_COMPILER="$cc" -DCMAKE_CXX_COMPILER="$cxxc" \
        #     -DLLVM_ENABLE_LIBCXX=ON                               \

        # Reduce build time by not building unneeded support for various ISAs.  Giving at least
        # one is required (even though `clangd` probably never generates machine code for it).
        case "${MY_PLATFORM_ARCH:?}" in
            (*86*|amd64) local targetArch=X86 ;;
            (*) error "Unsure which LLVM target ISA to choose." && return 4 ;;
        esac

        ( cd "$workDir" || exit
          set -o pipefail

          _my-download-https "$url" > "$file" || exit
          gnu sha256sum -c <<<"$checksum *$file"$'\n' || exit
          gnu tar -x -J -f "$file" || exit

          std mkdir build && cd build || exit

          cmake ../"$dir"/llvm -G Ninja                               \
                -DCMAKE_BUILD_TYPE=Release                            \
                -DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra"      \
                -DLLVM_TARGETS_TO_BUILD="${targetArch:?}"             \
                -DLLVM_BUILD_TOOLS=OFF                                \
                -DLLVM_ENABLE_BINDINGS=OFF                            \
                -DLLVM_INCLUDE_BENCHMARKS=OFF                         \
                -DLLVM_INCLUDE_EXAMPLES=OFF                           \
                -DLLVM_INCLUDE_TESTS=OFF                              \
                -DLLVM_INSTALL_UTILS=OFF                              \
            |& std tee generate-build-system.out || exit

          cmake --build . --target clangd |& std tee build.out || exit

          my-install-files --strip ./bin/clangd "$installDir"/bin/ || exit

        ) || return

        std rm -r -f "$workDir"
    fi
}


function my-build-and-install--gRPC
{
    local name=${1:?}
    [ "$name" = gRPC ] || exit

    if ! is-command-found grpc_cpp_plugin
    then
        local ver=1.59.1
        local file=v$ver.tar.gz
        local site=https://github.com/grpc/grpc
        local url=$site/archive/refs/tags/$file
        local checksum=916f88a34f06b56432611aaa8c55befee96d0a7b7d7457733b9deeacbc016f99
        local dir=grpc-$ver

        local thisFuncDefDir=$(std dirname "${BASH_SOURCE[0]}")
        local patch=$thisFuncDefDir/"${MY_PLATFORM_OS_VARIANT:?}"/grpc.patch

        local installDir=$(_my_platspec_install_dir)
        local workDir=$(gnu mktemp -d)

        my-platform-install-packages build-essential cmake ninja                        \
                                     protobuf openssl lib/c/c-ares lib/c++/{abseil,re2} \
            || return

        ( cd "$workDir" || exit
          set -o pipefail

          _my-download-https "$url" > "$file" || exit
          gnu sha256sum -c <<<"$checksum *$file"$'\n' || exit
          gnu tar -x -z -f "$file" || exit

          if [ -e "$patch" ]; then
              std patch -p 0 < "$patch" || exit
          fi
          std mkdir build && cd build || exit
          cmake ../"$dir" -G Ninja -DCMAKE_BUILD_TYPE=Release        \
                -DBUILD_SHARED_LIBS=ON                               \
                -DCMAKE_LIBRARY_ARCHITECTURE="${MY_PLATFORM_ARCH:?}" \
                --install-prefix "$installDir"                       \
            |& std tee generate-build-system.out || exit
          cmake --build . |& std tee build.out || exit
          cmake --install . --strip

        ) || return

        std rm -r -f "$workDir"
    fi
}


function my-build-and-install--bear
{
    local name=${1:?}
    [ "$name" = bear ] || exit

    if ! is-command-found "$name"
    then
        local ver=3.1.3
        local file=$ver.tar.gz
        local site=https://github.com/rizsotto/Bear
        local url=$site/archive/refs/tags/$file
        local checksum=8314438428069ffeca15e2644eaa51284f884b7a1b2ddfdafe12152581b13398
        local dir=Bear-$ver

        local thisFuncDefDir=$(std dirname "${BASH_SOURCE[0]}")
        local patch=$thisFuncDefDir/"${MY_PLATFORM_OS_VARIANT:?}"/bear.patch

        local installDir=$(_my_platspec_install_dir)
        local workDir=$(gnu mktemp -d)

        my-platform-install-packages build-essential cmake ninja         \
                                     protobuf gRPC lib/c++/nlohmann-json \
            || return

        prepend_to_PATH_if_ok            "$installDir"/bin
        prepend_to_LD_LIBRARY_PATH_if_ok "$installDir"/lib

        ( cd "$workDir" || exit
          set -o pipefail

          _my-download-https "$url" > "$file" || exit
          gnu sha256sum -c <<<"$checksum *$file"$'\n' || exit
          gnu tar -x -z -f "$file" || exit

          if [ -e "$patch" ]; then
              std patch -p 0 < "$patch" || exit
          fi
          std mkdir build && cd build || exit
          cmake ../"$dir" -G Ninja -DCMAKE_BUILD_TYPE=Release        \
                -DENABLE_UNIT_TESTS=OFF -DENABLE_FUNC_TESTS=OFF      \
                -DCMAKE_LIBRARY_ARCHITECTURE="${MY_PLATFORM_ARCH:?}" \
                -DCMAKE_PREFIX_PATH="$installDir"                    \
                --install-prefix "$installDir"                       \
            |& std tee generate-build-system.out || exit
          cmake --build . |& std tee build.out || exit
          cmake --install . --strip

        ) || return

        std rm -r -f "$workDir"
    fi
}


function my-install-emacs-packages
{
    function elisp-expr-to-call-prep-func {
        local func=${1:?}
        print "\
(let ((load-path (cons (concat user-emacs-directory \"my/lib\") load-path)))
  (when (and (require 'my-prepare-to-install-packages nil t)
             (fboundp '$func))
    ($func)))"
    }

    function do-emacs {
        local a args=(--batch --user='')  # Load the user's init, in batch mode.
        for a ; do args+=(--eval="$(elisp-expr-to-call-prep-func "$a")") ; done

        emacs "${args[@]}" --kill  # (End with --kill in case --batch is ever disabled.)
    }

    local name=${1:?}
    [ "$name" = my-emacs-packages ] || exit
    [ "${MY_PLATFORM_VARIANT-}" != NixOS ] || exit

    # Ensure that Emacs is installed.
    my-platform-install-packages emacs-nox || return

  ( export MY_EMACS_DISABLE_COMPILING_MY_DIR=true  # Avoid problems byte-compiling could cause.

    # First, configure things needed to install `use-package` and needed for the branch's use of
    # it.  (Such configuration is often not already present in my primary branches used in NixOS
    # (where instead I have Home Manager install the Emacs packages), and so doing this is needed
    # to prepare it for other OSs.)  (If `use-package-always-ensure` is already enabled and
    # `use-package` is already installed, this will instead do what the "final" step below
    # describes doing.)
    do-emacs my-prepare-needed-for-use-of-use-package || exit

    # Second, ensure that `use-package` is installed (needed for Emacs versions less-than 29).
    # (If `use-package` is already installed and `use-package-always-ensure` was enabled by the
    # "first" step above, this will instead do what the "final" step below describes doing, due to
    # `use-package-always-ensure` now being true.)
    do-emacs my-install-use-package || exit

    # Finally, allow `use-package`'s "ensure"ing to automatically install the packages specified
    # by the init files.  (If this was already done (per above comments) then this just does
    # nothing.)
    do-emacs
  ) || return

    # Further, byte-compile my `.el`s, now that the packages are installed (and assuming that the
    # init file will do so, now that the disabling env var is no longer present).  Not necessary,
    # but this is an appropriate point to do so and it fits with setting-up the libraries.
    do-emacs || return

    unset -f do-emacs elisp-expr-to-call-prep-func || exit
}
