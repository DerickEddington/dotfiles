;; -*- lexical-binding: t; -*-

;; ;; Can be useful for debugging:
;; (setq force-load-messages t)
;; (toggle-debug-on-error)
;; (debug)  ;; Must run `emacs' in terminal, else it aborts.
;; (debug-on-entry #'package-activate-all)  ;; ditto


;; To avoid seeing these even briefly.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


(when (version< emacs-version "27")
  (warn "Unsure if my config will work with this old Emacs version."))


(let ((load-path (cons (concat user-emacs-directory "my/lib") load-path)))
  (require 'my-platform))  ;; (Don't use `use-package` yet, in case it's not available yet.)

;; Get my scheme of platform-specific identifiers and XDG-BDS locations.
(defconst my-platform (my-platform-info) "The values of `$MY_*' of `my/sh/helpers.sh'")


;; Compute these dynamically based on the current environment variables, instead of hard-coding
;; values in `custom.el' or elsewhere.  Must be done here, in the early-init file, not in
;; `./init/xdg-bds.el', so these take effect ASAP for the startup phase between here and the
;; beginning of the normal init file, which includes the loading of the site-wide startup file
;; (and includes the expressions in this file after here).  We don't use
;; `startup-redirect-eln-cache' (which is only available in Emacs 29+) because the above already
;; changed the variable (and because that function isn't in Emacs 28).  Must not use `setopt'
;; (which is only available in Emacs 29+) nor other parts of Customize because it's too early for
;; those.
;;
(when (featurep 'native-compile)
  (defconst my-eln-cache-dir (concat (alist-get 'cache-home my-platform)
                                     "my/emacs/platform/"
                                     (alist-get 'os-var-ver-arch my-platform)
                                     "/eln-cache/"))
  ;; Remove the `~/.config/emacs/eln-cache/' element that the earlier startup placed as first.
  (setq native-comp-eln-load-path (cdr native-comp-eln-load-path))
  ;; Replace the original first element with our platform-specific dir that's under `~/.cache/'.
  (push my-eln-cache-dir native-comp-eln-load-path))
(setq
 package-user-dir (concat (alist-get 'data-home my-platform)
                          "my/emacs/elpa/"))
