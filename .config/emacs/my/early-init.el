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
;; `./init/xdg-bds.el', so these take effect ASAP.  Must not use `setopt' (nor other parts of
;; Customize) because it's too early for those.
;;
(when (featurep 'native-compile)
  (defconst my-eln-cache-dir (concat (alist-get 'cache-home my-platform)
                                     "my/emacs/platform/"
                                     (alist-get 'os-var-ver-arch my-platform)
                                     "/eln-cache/"))
  (startup-redirect-eln-cache my-eln-cache-dir))
(setq
 package-user-dir (concat (alist-get 'data-home my-platform)
                          "my/emacs/elpa/"))
