{ config, pkgs, lib, myLib, ... }:

let
  inherit (builtins) replaceStrings;
  inherit (lib) mkIf;
  inherit (lib.attrsets) recursiveUpdate;
  inherit (myLib) nixosConfigLoc sourceCodeOfPkg;

  # Get the raw definition of the `debugging` module from my system-wide NixOS directory.
  systemWideDebuggingModule = rec {
    loc =
      assert nixosConfigLoc.isDefined;
      nixosConfigLoc.dirName + "/debugging.nix";
    value =
      import loc {
        inherit
          config  # The Home Manager `config` (as opposed to the NixOS `config`).
          pkgs lib
          myLib;  # (Note: `myLib` extends the system-wide `myLib`.)
      };
  };
  sysWide = systemWideDebuggingModule.value;
in

{
  # Reuse the raw definition, but change an asepct, of `options.my.debugging` from system-wide.
  options.my.debugging = recursiveUpdate sysWide.options.my.debugging {
    # Reword to be relevant to user context instead.
    support.sourceCode.of.prebuilt.packages.description =
      replaceStrings ["environment.systemPackages"] ["home.packages"]
        sysWide.options.my.debugging.support.sourceCode.of.prebuilt.packages.description;
  };

  config = let
    cfg = config.my.debugging;
  in {
    # Reuse the raw definition, but change an asepct, of `config.my.debugging` from system-wide.
    my.debugging = recursiveUpdate sysWide.config.my.debugging {
      # Don't use those that are for the system-wide configuration (e.g. `hostLibc`), and instead
      # use our own selection that is for the user configuration.
      support.sourceCode.of.prebuilt.packages = [
        # Could add from `pkgs` here, to be common to all users.
      ];
    };

    home = {
      # Automatically install the "debug" output of packages if they have one, and set the
      # NIX_DEBUG_INFO_DIRS environment variable to include them, for GDB to find them.
      enableDebugInfo = cfg.support.debugInfo.of.prebuilt.enable;

      packages = mkIf cfg.support.sourceCode.of.prebuilt.enable
        (map sourceCodeOfPkg.only
             cfg.support.sourceCode.of.prebuilt.packages);

      # (Note: Unlike NixOS's `environment.pathsToLink`, Home Manager doesn't seem to have such an
      # option, and it seems to always link to the derivations' /nix/store/.../src/ from
      # ~/.nix-profile/src/.  This works-out for where my custom approach places the source-code
      # files - in a derivation's output directory under ./src/.)
    };
  };
}
