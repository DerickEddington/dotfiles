{ config, pkgs, lib, ... }:

let
  inherit (builtins) concatMap foldl';
  inherit (lib) mkOption types;

  unstableEmacsFiles = rec {
    channel = <nixos-unstable>;
    pkg = "pkgs/applications/editors/emacs";
    sub = x: channel + "/${pkg}/${toString x}";
  };
in

{
  options.my.emacs = with types; {

    extraPackages = mkOption {
      type = listOf (functionTo (listOf package));
      default = [];
    };

    elpa = {
      generated = mkOption {
        type = nullOr path;
        # By default, use the latest list of what ELPA has, so that updated
        # versions of Emacs packages in ELPA are available to us sooner.  The
        # stable <nixos> channel is not updated after release, whereas the
        # <nixos-unstable> channel is continually updated.
        default = let
          archive = "elisp-packages/elpa-generated.nix";
        in
          unstableEmacsFiles.sub archive;
      };
    };

    melpa = {
      archiveJson = mkOption {
        type = nullOr path;
        # By default, use the latest list of what MELPA has, so that updated
        # versions of Emacs packages in MELPA are available to us sooner.  The
        # stable <nixos> channel is not updated after release, whereas the
        # <nixos-unstable> channel is continually updated.
        default = let
          archive = "elisp-packages/recipes-archive-melpa.json";
        in
          unstableEmacsFiles.sub archive;
      };
    };

    overrides = mkOption {
      type = listOf (functionTo raw);  # (Unsure why attrsOf wouldn't work.)
      default = [];
    };
  };

  config = {
    programs.emacs = {
      enable = true;
      package = pkgs.emacs29-gtk3;
      extraPackages = epkgs: concatMap (f: f epkgs) config.my.emacs.extraPackages;
      overrides = self: super:
        let listOfAttrs = map (f: f self super) config.my.emacs.overrides;
        in foldl' (a: b: a // b) {} listOfAttrs;
    };

    my.emacs = {
      extraPackages = [(epkgs: with epkgs; [
        adaptive-wrap
        all-the-icons
        charmap
        cmake-mode
        company
        counsel
        counsel-fd
        counsel-tramp
        expand-region
        flycheck
        fd-dired
        git-modes
        home-end
        ibuffer-project
        ivy
        ivy-hydra
        lsp-ivy
        lsp-mode
        lsp-ui
        lua-mode
        magit
        multiple-cursors
        nix-mode
        rg
        rust-mode
        smartparens
       #tramp  # To use version from ELPA that is newer than built-in.
        toml-mode
        yaml-mode
      ])];

      # This `overrides` (used with `overrideScope'`) causes these exact
      # emacs-package derivations (that are our custom "latest ELPA & MELPA"
      # ones) to also be used to satisfy the dependencies of any emacs-packages
      # that depend on these emacs-packages' names.  Otherwise, multiple
      # versions of some emacs-packages might be installed, e.g. the latest
      # `ivy` that we give here and another older-version `ivy` (from the
      # original emacs-packages set) since `counsel` also depends on it.  That
      # is avoided by this overriding, e.g. only the single latest `ivy` is
      # installed and is also used for the dependency of `counsel`.
      overrides = [
        # Maybe use our custom archive of ELPA.
        (self: super: let
          inherit (config.my.emacs.elpa) generated;
          elpaChoice =
            # Maybe use our custom archive.
            if generated == null
            then { inherit (super) elpaPackages; }
            else { elpaPackages = super.elpaPackages.override { inherit generated; }; };
          inherit (elpaChoice) elpaPackages;
        in (
          # Attribute that enables named unambiguous access to this set without
          # regard to how the below orders its members at the top-level.
          { inherit elpaPackages; }
          # All from ELPA, with lower priority than MELPA (because this override
          # function is before the below).
          // elpaPackages
        ))
        # Maybe use our custom archive of MELPA; and control the order of Stable
        # versus Unstable MELPA.
        (self: super: let
          inherit (config.my.emacs.melpa) archiveJson;
          melpaChoice =
            # Maybe use our custom archive.
            if archiveJson == null
            then { inherit (super) melpaPackages melpaStablePackages; }
            else let
              melpaOverride = { inherit archiveJson; };
            in {
              melpaPackages       = super.melpaPackages.override       melpaOverride;
              melpaStablePackages = super.melpaStablePackages.override melpaOverride;
            };
          inherit (melpaChoice) melpaPackages melpaStablePackages;
        in (
          # Attributes that enable named unambiguous access to these different
          # sets without regard to how the below orders their members at the
          # top-level.
          { inherit melpaPackages melpaStablePackages; }

          # Control the order of Stable versus Unstable, for all non-particular
          # packages.
          // (
            # All from latest Stable.
            melpaStablePackages //
            # All from latest Unstable, with higher priority than Stable.
            melpaPackages
          )
        ))
        # Control the order of Stable versus Unstable, for select particular
        # packages.
        (self: super:
          # Particular ones from latest Stable, with higher priority than
          # Unstable.
          {
            inherit (self.melpaStablePackages)
            ;
          }
        )
        # Custom packages that are modifications of existing or are new additions.
        (self: super:
          {
            lsp-mode = self.melpaPackages.lsp-mode.overrideAttrs (oldAttrs: {
              # Always compile and load `lsp-mode` with `lsp-use-plists` set to
              # true, because it is too difficult to set the `LSP_USE_PLISTS`
              # env-var in both the compile-time and the load-time of the
              # `lsp-mode` package (as would be needed otherwise).
              postPatch =
                (oldAttrs.postPatch or "")
                + (let orig = ''^( *)\(defvar +(lsp-use-plists) +\(getenv +"LSP_USE_PLISTS"\)\)'';
                       new = ''\1;; Patched by Derick to always be true.\n\1(defconst \2 t)'';
                   in ''sed -i -E -e 's/${orig}/${new}/' lsp-protocol.el'');
            });
          }
        )
      ];
    };
  };
}
