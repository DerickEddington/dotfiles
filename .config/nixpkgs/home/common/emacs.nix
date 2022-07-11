{ config, pkgs, lib, ... }:

let
  inherit (builtins) concatMap foldl';
  inherit (lib) mkOption types;
in

{
  options.my.emacs = with types; {

    extraPackages = mkOption {
      type = listOf (functionTo (listOf package));
      default = [];
    };

    melpa = {
      archiveJson = mkOption {
        type = nullOr path;
        # By default, use the latest list of what MELPA has, so that updated
        # versions of Emacs packages in MELPA are available to us sooner.  The
        # stable <nixos> channel is not updated after release, whereas the
        # <nixos-unstable> channel is continually updated.
        default = let
          channel = <nixos-unstable>;
          pkg = "pkgs/applications/editors/emacs";
          archive = "elisp-packages/recipes-archive-melpa.json";
        in
          channel + "/${pkg}/${archive}";
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
      package = pkgs.emacsNativeComp;
      extraPackages = epkgs: concatMap (f: f epkgs) config.my.emacs.extraPackages;
      overrides = self: super:
        let listOfAttrs = map (f: f self super) config.my.emacs.overrides;
        in foldl' (a: b: a // b) {} listOfAttrs;
    };

    my.emacs = {
      extraPackages = [(epkgs: with epkgs; [
        adaptive-wrap
        all-the-icons
        cargo
        company
        counsel
        expand-region
        flycheck
        home-end
        ibuffer-project
        ivy
        ivy-hydra
        lsp-ivy
        lsp-mode
        lsp-ui
        magit
        multiple-cursors
        nix-mode
        rg
        rust-mode
        smartparens
        toml-mode
      ])];

      # This `overrides` (used with `overrideScope'`) causes these exact
      # emacs-package derivations (that are our custom "latest MELPA" ones) to
      # also be used to satisfy the dependencies of any emacs-packages that
      # depend on these emacs-packages' names.  Otherwise, multiple versions of
      # some emacs-packages might be installed, e.g. the latest `ivy` that we
      # give here and another older-version `ivy` (from the original
      # emacs-packages set) since `counsel` also depends on it.  That is avoided
      # by this overriding, e.g. only the single latest `ivy` is installed and
      # is also used for the dependency of `counsel`.
      overrides = [
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
              magit
            ;
          }
        )
      ];
    };
  };
}
