;; -*- lexical-binding: t; -*-

;; ;; Can be useful for debugging:
;; (setq force-load-messages t)
;; (toggle-debug-on-error)
;; (debug)  ;; Must run `emacs' in terminal, else it aborts.
;; (debug-on-entry #'package-activate-all)  ;; ditto


(when (featurep 'native-compile)
  ;; Disable native compilation during the below until ready to re-enable it.  This prevents
  ;; attempting writing (and wasting work compiling) new `.eln's for any libraries loaded during
  ;; the below that don't already have a `.eln', which is needed for Debian 12's Emacs 28 and
  ;; possibly other OSs where not having a writable first element of `native-comp-eln-load-path'
  ;; (as done next) would cause an error.
  (if (boundp 'native-comp-jit-compilation)
      (setq native-comp-jit-compilation nil)      ;; Emacs 29+
    (setq native-comp-deferred-compilation nil))  ;; Emacs 28
  ;; Remove the `~/.config/emacs/eln-cache/' element that the earlier startup placed as first.
  ;; This also prevents writing any new `.eln's (for Emacs 29+ but not 28 it seems) during the
  ;; below until the further changing of this variable is ready below, and this prepares the value
  ;; for that changing.  (We don't add some temporary dummy dir as first element because, while it
  ;; would work to have a writable one, it would cause recompiling every time Emacs starts every
  ;; time the temp dir was removed, and it would place such `.eln's in an undesirable location
  ;; different from where the rest are.)
  (setq native-comp-eln-load-path (cdr native-comp-eln-load-path)))


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
  ;; Replace the original first element with our platform-specific dir that's under `~/.cache/'.
  (push my-eln-cache-dir native-comp-eln-load-path)
  ;; Re-enable native compilation, now that `native-comp-eln-load-path' is ready.  (For the
  ;; unlikely case that the Emacs build actually had this as `nil' originally but has the
  ;; `native-compile' feature, this will actually enable native compilation when it would've been
  ;; disabled otherwise, which is also what I'd want.)
  (if (boundp 'native-comp-jit-compilation)
      (setq native-comp-jit-compilation t)       ;; Emacs 29+
    (setq native-comp-deferred-compilation t)))  ;; Emacs 28

(setq
 package-user-dir (concat (alist-get 'data-home my-platform)
                          "my/emacs/elpa/"))


(when noninteractive       ;; Emacs is running in `--batch --user' mode.
  (package-activate-all))  ;; Because batch mode doesn't do this (unlike normal mode which does).
