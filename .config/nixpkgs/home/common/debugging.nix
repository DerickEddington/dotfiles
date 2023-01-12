{ config, pkgs, lib, myLib, ... }:

let
  inherit (builtins) replaceStrings;
  inherit (lib.attrsets) recursiveUpdate;
  inherit (myLib) nixosConfigLoc;

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
    inherit (builtins) elem listToAttrs;
    inherit (lib) mkDefault mkIf;
    inherit (lib.strings) concatMapStringsSep optionalString;
    inherit (myLib) nixos sourceCodeOfPkg;
    inherit (myLib.tmpfiles.debugging) mkDebugInfoDirPkg mkSourceCodeDirPkg;

    cfg = config.my.debugging;
    enabled.anySourceCode = let inherit (cfg.support) sourceCode;
                            in (    sourceCode.all.enable
                                 || sourceCode.of.prebuilt.enable
                                 || sourceCode.of.locallyBuilt.enable);
  in {
    # Reuse the raw definition, but change aspects, of `config.my.debugging` from system-wide.
    my.debugging = recursiveUpdate sysWide.config.my.debugging {
      support = {
        debugInfo.tmpDirs = mkDefault (if cfg.support.debugInfo.all.enable
                                       then [~/tmp/debug]
                                       else []);
        sourceCode = {
          # Don't use those that are for the system-wide configuration (e.g. `hostLibc`), and
          # instead use our own selection that is for the user configuration.
          of.prebuilt.packages = [
            # Could add from `pkgs` here, to be common to all users.
          ];
          tmpDirs = mkDefault (if cfg.support.sourceCode.all.enable
                               then [~/tmp/src]
                               else []);
        };
      };
    };

    home = {
      # Automatically install the "debug" output of packages if they have one, and set the
      # NIX_DEBUG_INFO_DIRS environment variable to include them, for GDB to find them.
      enableDebugInfo = cfg.support.debugInfo.of.prebuilt.enable;

      sessionVariablesExtra = let
        # Extend NIX_DEBUG_INFO_DIRS.  Must use sessionVariablesExtra because sessionVariables is
        # types.attrs which silently drops earlier attribute definitions and so would not work for
        # this.  (That type is deprecated because of that.)
        NIX_DEBUG_INFO_DIRS = let
          systemWide = "\${NIX_DEBUG_INFO_DIRS:+:}$NIX_DEBUG_INFO_DIRS";
          underHome = concatMapStringsSep ":" toString cfg.support.debugInfo.tmpDirs;
        in
          ''NIX_DEBUG_INFO_DIRS="${underHome}${systemWide}"'';
      in
        mkIf (cfg.support.debugInfo.tmpDirs != []) ''
          export ${NIX_DEBUG_INFO_DIRS}
        '';

      file = {
        ".config/gdb/my-source-path-init".text = let
          sys.hasSrc = elem "/src" nixos.config.environment.pathsToLink;
          sys.srcLoc = optionalString sys.hasSrc "/run/current-system/sw/src/of-pkg-via-my";
          sys.hasTmp = nixos.config.my.debugging.support.sourceCode.tmpDirs != [];
          sys.tmpLoc = toString nixos.config.my.debugging.support.sourceCode.tmpDirs;
          user.hasSrc = enabled.anySourceCode;
          user.srcLoc = optionalString user.hasSrc "~/.nix-profile/src/of-pkg-via-my";
          user.hasTmp = cfg.support.sourceCode.tmpDirs != [];
          user.tmpLoc = toString cfg.support.sourceCode.tmpDirs;
        in ''
          # Useful locations to have in the "source path".  (Note that `dir` adds to the front
          # (pushes), so these lines are in reverse order of precedence, but within a line the
          # order is the precedence.)

          ${optionalString (sys.hasSrc || user.hasSrc) ''
              # These work with my custom NixOS and Home Manager configurations that automatically
              # manage these.  Most Nixpkgs' source files are unpacked and prepared in their own
              # /build/ directory, and my pkgWithDebuggingSupport copies build directories to
              # /run/current-system/sw/src/of-pkg-via-my/ or ~/.nix-profile/src/of-pkg-via-my/.
              dir ${user.srcLoc} ${sys.srcLoc}
            ''}

          ${optionalString (sys.hasTmp || user.hasTmp) ''
              # These are managed by the user.
              dir ${user.tmpLoc} ${sys.tmpLoc}
            ''}
        '';
      }
      //
      (listToAttrs (map
        ({ pkg, destPath }: {
          name = ".config/user-tmpfiles.d/${baseNameOf destPath}";
          value = { source = pkg + destPath; };
        })
        ((map (mkDebugInfoDirPkg  {}) cfg.support.debugInfo.tmpDirs) ++
         (map (mkSourceCodeDirPkg {}) cfg.support.sourceCode.tmpDirs))));

      packages = mkIf cfg.support.sourceCode.of.prebuilt.enable
        (map sourceCodeOfPkg.only
             cfg.support.sourceCode.of.prebuilt.packages);

      # (Note: Unlike NixOS's `environment.pathsToLink`, Home Manager doesn't seem to have such an
      # option, and it seems to always link to the derivations' /nix/store/.../src/ from
      # ~/.nix-profile/src/.  This works-out for where my custom approach places the source-code
      # files - in a derivation's output directory under ./src/.)
    };

    # Enable the tmpfiles user units for systemd.
    systemd.user.tmpfiles.rules = let
      hasDbg = cfg.support.debugInfo.tmpDirs  != [];
      hasSrc = cfg.support.sourceCode.tmpDirs != [];
      never = "never-will-exist-mZwKT8DRkfYbYk1tFQYjiFgjKzXXGaGTol87GpwHxY7CIPgOC2aKnFmMcbCDq9N7";
    in
      mkIf (hasDbg || hasSrc) ["r /tmp/${never}"];  # Just need a no-op entry, to be non-empty.
  };
}
