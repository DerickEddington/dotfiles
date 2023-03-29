{ config, pkgs, lib, ... }:

let
  inherit (builtins) pathExists;
  inherit (lib) mkDefault mkEnableOption mkIf mkOption types;
  inherit (lib.attrsets) mapAttrs' mapAttrsToList;
  inherit (lib.lists) flatten;
  inherit (lib.strings) escapeShellArg;
  inherit (pkgs) runCommandLocal symlinkJoin;
  inherit (pkgs) rust-bin;  # (Assumes my system-wide overlay added this.)

  cfg = config.my.rust;
in
{
  options.my.rust = {
    enable = mkEnableOption "my setup for developing with Rust";
    toolchains = mkOption {
      description = ''
        Rust toolchains, from derivations, to make available
        via Rustup custom toolchains.
      '';
      type = with types; attrsOf package;
    };
    defaultToolchain = mkOption {
      description = ''
        The default out of `my.rust.toolchains`.
        Affects things like which toolchain's tab-completion for shells is used.
      '';
      type = types.enum (mapAttrsToList (n: v: n) cfg.toolchains);
      default = "the-stable";
    };
    packages = mkOption {
      description = "Tools for development with Rust that I like to have.";
      type = with types; listOf package;
      default = (with pkgs; [
        (assert ! pathExists "${rustup}/bin/rust-analyzer";
         # If the rustup package ever provides this itself, I'll want to use that instead.
         rust-bin.stable.latest.rust-analyzer)
      ]) ++ (with pkgs.unstable; [  # From unstable Nixpkgs, for newer versions.
        cargo-binutils
        cargo-readme
        evcxr
      ]);
    };
  };

  config = let
    user = config.home.username;
  in
    mkIf cfg.enable {

      home.packages = (with pkgs; [
        rustup  # Not optional.
      ])
      ++ cfg.packages
      ++
      # This symlinks toolchains' standard-library's debug-info's "compilation directory"
      # structures from the ~/.nix-profile/src/of-pkg-via-my/ of my debugging-support design, so
      # that debugging of binaries produced by these toolchains will automatically find the
      # source-code for the corresponding Rust Standard Library, when available.
      (mapAttrsToList
        # TODO: Maybe move to a function in myLib system-wide.
        (name: toolchainDrv:
          runCommandLocal "srcdir-for-rustup-custom-toolchain--${name}"
            { nativeBuildInputs = with pkgs; [ dwarfdump ripgrep ]; }
            ''
              shopt -s nullglob
              set -o errexit -o nounset -o pipefail # -o xtrace

              function cancel { mkdir "$out"; exit 0; }  # Just produce empty output.

              toolchain=${escapeShellArg toolchainDrv}
              rustlib="$toolchain/lib/rustlib"
              rustSrc="$rustlib/src/rust"
              [[    -d "$rustSrc/library/std"  # Their newer layout.
                 || -d "$rustSrc/src/libstd"   # Their older layout.
              ]] || cancel  # If not, no std-lib sources in this given toolchain, so do nothing.

              targetTriple=${escapeShellArg pkgs.stdenv.hostPlatform.config}
              pushd "$rustlib/$targetTriple/lib"
              libstd=(libstd-*.so)
              [ ''${#libstd[@]} -eq 1 ]  # If not, fail intentionally.
              compDir=()
              for rx in '^library/std/src/lib\.rs' '^src/libstd/lib\.rs' ; do
                compDir+=(
                  $(dwarfdump --search-regex="$rx" --search-print-parent --format-dense \
                              "$libstd"                                                 \
                    | (rg --replace '$1' --only-matching --regexp ' DW_AT_comp_dir<(.+?)> ' \
                       || true)))
              done
              popd
              [ ''${#compDir[@]} -eq 1 ]  # If not, fail intentionally.

              outSubDir="$out/src/of-pkg-via-my/$(dirname "$compDir")"
              mkdir -p "$outSubDir"
              ln -v -s "$rustSrc" "$outSubDir/$(basename "$compDir")"
            '')
        cfg.toolchains);

      home.file = mapAttrs'
        (name: toolchainDrv: {
          name = ".rustup/toolchains/${name}";
          value = {
            source = toolchainDrv;
          };
        })
        cfg.toolchains;

      # This is needed for Bash to find the tab-completion configuration for Cargo.
      xdg.systemDirs.data = [ "${cfg.toolchains.${cfg.defaultToolchain}}/share" ];

      my.rust = {
        # Defaults that a user may supersede.
        toolchains = let
          targets = [
            # Extras for cross compilation.  E.g.:
          # "riscv32im-unknown-none-elf"
          # "x86_64-unknown-linux-musl"
          ];

          # TODO: Maybe move to myLib system-wide.
          fromNixpkgsRustPackages = rustPackages: extraSelector:
            symlinkJoin {
              name = "from-Nixpkgs-rustPackages-toolchain-${rustPackages.rustc.version}";
              paths = (with rustPackages; [
                rustc
                cargo
              ]) ++ (extraSelector rustPackages);
            };
        in {
          # Rustup custom toolchain names must not conflict with its builtin names (i.e. "stable",
          # "nightly", etc.), and so different names must be used here.

          # The Stable that was most-recently released.
          the-stable = mkDefault (rust-bin.stable.latest.default.override {
            extensions = [
              "llvm-tools-preview"
              "rust-analyzer"
              "rust-src"
            ];
            inherit targets;
          });

          # The Nightly from the same date as the release of `the-stable`, so this derivation is
          # only updated when that is (otherwise the latest would be updated every day).
          the-nightly = let
            dateOfStable = rust-bin.manifests.stable.latest.date;
          in
            mkDefault (rust-bin.nightly.${dateOfStable}.minimal.override {
              extensions = [
                "clippy"
                "rustfmt"
              ];
              inherit targets;
            });

          # The Rust toolchain that is provided by stable Nixpkgs.
          nixos = mkDefault
            (fromNixpkgsRustPackages pkgs.rustPackages
              (rustPackages: (with rustPackages; [
              # clippy
              # rustfmt
              ]) ++ (with pkgs; [
              # rust-analyzer  # From Nixpkgs (not the `rust-bin` above).

                # Don't know if it's possible to have a corresponding llvm-tools-preview.

                # Note that I usually have the source-code for this toolchain already installed at
                # /run/current-system/sw/src/of-pkg-via-my/build/rustc-$VER-src/ via my NixOS
                # option my.debugging.support.sourceCode.of.prebuilt.packages (e.g. defined in
                # /etc/nixos/per-host/$HOSTNAME/default.nix).
              ])));

          # # The Rust toolchain that is provided by unstable Nixpkgs.
          # nixos-unstable = mkDefault
          #   (fromNixpkgsRustPackages pkgs.unstable.rustPackages
          #     (rustPackages: (with rustPackages; [
          #     ]) ++ (with pkgs; [
          #     ])));
        };
      };

      # Customize the locations of the large dispensable sub-directories of ~/.rustup/ and
      # ~/.cargo/ to be outside of the home directory, so their contents are excluded from
      # backups.  Note: it's still possible for a user to reorganize these sub-dirs to be located
      # somewhere else some other way, and these tmpfiles rules will not disturb those when they
      # already exist.
      systemd.user.tmpfiles.rules = [
        "q /mnt/omit/%h                    0700 ${user} users -"
        "q /var/tmp/%h                     0700 ${user} users 30d"

        "d /mnt/omit/%h/.rustup            -    ${user} users -"
        "d /mnt/omit/%h/.rustup/toolchains -    -       -     -"
        "d /mnt/omit/%h/.rustup/tmp        -    -       -      1d"  # Must be same device.
        "d /var/tmp/%h/.rustup             -    ${user} users 30d"
        "d /var/tmp/%h/.rustup/downloads   -    -       -     30d"
        "d %h/.rustup            - - - -"
        "L %h/.rustup/toolchains - - - - /mnt/omit%h/.rustup/toolchains"
        "L %h/.rustup/tmp        - - - - /mnt/omit%h/.rustup/tmp"
        "L %h/.rustup/downloads  - - - - /var/tmp%h/.rustup/downloads"

        "d /var/tmp/%h/.cargo              -    ${user} users 30d"
        "d /var/tmp/%h/.cargo/registry     -    -       -     30d"
        "d /var/tmp/%h/.cargo/git          -    -       -     30d"
        "d %h/.cargo             - - - -"
        "L %h/.cargo/registry    - - - - /var/tmp%h/.cargo/registry"
        "L %h/.cargo/git         - - - - /var/tmp%h/.cargo/git"
      ];

      home.activation = let
        inherit (lib.hm) dag;
      in {
        # When ~/.rustup/toolchains does not exist, must create it before linkGeneration, to
        # prevent linkGeneration from creating it as a normal directory, so that it's created as
        # the symlink according to the user-tmpfiles rule above.
        myRustupToolchainsDirWhenMissing = dag.entryBetween ["linkGeneration"]
                                                            ["writeBoundary"] ''
          if ! [ -e ~/.rustup/toolchains ] && ! [ -L ~/.rustup/toolchains ]
          then
            [ -v VERBOSE ] && echo "Will create missing ~/.rustup/toolchains"

            $DRY_RUN_CMD  ${pkgs.systemd}/bin/systemd-tmpfiles --user --create      \
                                                --prefix="/mnt/omit/$HOME/.rustup"  \
                                                --prefix="$HOME/.rustup/toolchains"

            # For the very-first time my.rust.enable is enabled, our tmpfiles rules don't exist
            # yet, and so our systemd-tmpfiles command above does nothing, and so we must create
            # the symlink manually in this very-rare (but important) case.
            if ! [ -e ~/.rustup/toolchains ]; then
              $DRY_RUN_CMD  mkdir $VERBOSE_ARG -p -m 0700 /mnt/omit/$HOME
              $DRY_RUN_CMD  mkdir $VERBOSE_ARG -p /mnt/omit/$HOME/.rustup/toolchains ~/.rustup
              $DRY_RUN_CMD  ln $VERBOSE_ARG -s -T /mnt/omit$HOME/.rustup/toolchains \
                                                  ~/.rustup/toolchains
            fi
          fi
        '';
      };
    };
}
