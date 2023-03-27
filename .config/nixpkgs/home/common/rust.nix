{ config, pkgs, lib, ... }:

let
  inherit (lib) mkDefault mkEnableOption mkIf mkOption types;
  inherit (lib.attrsets) mapAttrs' mapAttrsToList;
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
  };

  config = let
  in
    mkIf cfg.enable {

      home.packages = (with pkgs; [
        rustup
      ])
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
    };
}
