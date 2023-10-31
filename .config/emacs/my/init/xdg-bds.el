;; -*- lexical-binding: t; -*-

;; Change Emacs' settings to follow the XDG Base Directory Specification as much as possible.
;; It's likely that this should be added to as further settings are recognized as appropriate.
;; https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html

(eval-when-compile (defvar my-platform))  ;; Silence byte compiler.


(defconst my-cache-dir (concat (alist-get 'cache-home my-platform) "/my/emacs"))
(defconst my-state-dir (concat (alist-get 'state-home my-platform) "/my/emacs"))


;; These are "built-in", i.e. not `provide'd by a library, which is why `use-package' isn't used.
(setopt
 auto-save-list-file-prefix (concat my-state-dir "/auto-save-list/.saves-"))


;; These do `:config (setopt ...)' because `:custom' wouldn't always work (e.g. that wouldn't work
;; for `tramp-persistency-file-name' due to `tramp-loaddefs'.)

(use-package savehist :config
  (setopt
   savehist-file (concat my-state-dir "/history")))

(use-package tramp-cache :ensure nil :config
  (setopt
   tramp-persistency-file-name (concat my-cache-dir "/tramp")))

(use-package tramp :config
  (setopt
   tramp-auto-save-directory (concat my-state-dir "/tramp-auto-save")))

(use-package transient :config
  (setopt
   transient-history-file (concat my-state-dir "/transient/history.el")))

(use-package url-cache :ensure nil :config
  (setopt
   url-cache-directory (concat my-cache-dir "/url/cache")))

(use-package url-cookie :ensure nil :config
  (setopt
   url-cookie-file (concat my-state-dir "/url/cookies")))

(use-package url-history :ensure nil :config
  (setopt
   url-history-file (concat my-state-dir "/url/history")))
