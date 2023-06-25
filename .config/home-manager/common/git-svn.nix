{ config, lib, pkgs, ... }:

let
  inherit (builtins) length;
  inherit (lib) mkDefault mkIf mkOption types;

  cfg = config.my.git-svn;
in

{
  options.my.git-svn = {
    directories = mkOption {
      description = ''
        Directories in which special configuration for git-svn clones should be applied.
        If the value has one or more elements, then `pkgs.gitSVN` is chosen for
        `programs.git.package`, unless defined elsewhere.
      '';
      # The element type is `str`, not `path`, because it needs to support both
      # having or not having a trailing slash, because that distinction is
      # semantically significant for the `gitdir` keyword of Git's `includeIf`.
      type = with types; listOf str;
      default = [];
    };
  };

  config = mkIf (length cfg.directories >= 1) {
    programs.git = {
      # Include git-svn with Git.  (My global config does not.)
      # Or could be: `pkgs.gitFull` or `p.override { svnSupport = true; }`.
      package = mkDefault pkgs.gitSVN;

      # "Include" sections to have in the Git config.  This Home Manager option
      # is like `extraConfig` but more convenient for "includes".
      includes = let
        # For repos that use `git-svn`.  (Uses the `includeIf` of Git config.)
        svnConfig = gitdir: {
          condition = "gitdir:${gitdir}";
          contents = {
            # Tell Magit to auto-enable `magit-svn-mode`.
            magit.extension = "svn";

            # For `git-svn`.  See `man git-svn`.
            svn = {
              useLogAuthor = true;
            # addAuthorFrom = true;
            # authorsFile = ~/.../authors;  # TODO: Or does it have to be `authorsfile`?
            # authorsProg = ...;
            };
            # TODO: Might not merge with same section as created by `git svn clone` etc.
          # svn-remote.${name} = { ...; pushurl = ...; ...; };

            # Have porcelain like `git diff` and `git log` detect copies, since
            # they are more reified in Subversion.  Also causes
            # `status.renames=copies` and `merge.renames=true`.
            diff.renames = "copies";

            # Need to avoid Git-merges with `git-svn`.  Some of these, like the
            # `pull` and `push` sections, are not very needed, because `git-svn`
            # creates repos without any Git-remotes (instead it has its own type
            # of svn-remote), and so those operations usually do nothing.  It's
            # still good to have such sections, to be cautious in case such
            # operations are somehow done in some weird situation.
            merge = {
              # Does not prevent merge commits (because giving `--no-ff` will
              # override), but is helpful in that doing merges without an "FF"
              # argument will default to be like `--ff-only`.
              ff = "only";
            };
            pull = {
              rebase = true;
              ff = "only";  # Redundant with `merge.ff=only`, I think, but why not.
            };
          # branch.autoSetupRebase = "always";  # TODO: Not needed with `pull.rebase=true`, I think
            push = {
              # Does not prevent pushes, but is somewhat helpful in that
              # attempting pushes without a refspec will error-out and require
              # giving a refspec explicitly, which might help remind me not to
              # push at all.
              default = "nothing";
            };
          };
        };
      in
        map svnConfig cfg.directories;
    };

    my.emacs.extraPackages = [(epkgs:
      with epkgs; [
        magit-svn
      ]
    )];
  };
}
