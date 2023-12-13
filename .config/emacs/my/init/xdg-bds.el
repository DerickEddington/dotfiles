;; -*- lexical-binding: t; -*-

;; Change Emacs' settings to follow the XDG Base Directory Specification as much as possible.
;; It's likely that this should be added to as further settings are recognized as appropriate.
;; https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html

(eval-when-compile (defvar my-platform))  ;; Silence byte compiler.


(defconst my-cache-dir (concat (alist-get 'cache-home my-platform) "/my/emacs"))
(defconst my-state-dir (concat (alist-get 'state-home my-platform) "/my/emacs"))


;; These are "built-in", i.e. not `provide'd by a library, which is why `eval-after-load' isn't
;; used.
(setopt
 auto-save-list-file-prefix (concat my-state-dir "/auto-save-list/.saves-"))


;; These use `eval-after-load' directly (instead of using `use-package') so that these still work
;; when `use-package' is unavailable (e.g. not yet installed).

(eval-after-load 'savehist
  (setopt
   savehist-file (concat my-state-dir "/history")))

(eval-after-load 'tramp-cache
  (setopt
   tramp-persistency-file-name (concat my-cache-dir "/tramp")))

(eval-after-load 'tramp
  (setopt
   tramp-auto-save-directory (concat my-state-dir "/tramp-auto-save")))

(eval-after-load 'transient
  (setopt
   transient-history-file (concat my-state-dir "/transient/history.el")))

(eval-after-load 'url-cache
  (setopt
   url-cache-directory (concat my-cache-dir "/url/cache")))

(eval-after-load 'url-cookie
  (setopt
   url-cookie-file (concat my-state-dir "/url/cookies")))

(eval-after-load 'url-history
  (setopt
   url-history-file (concat my-state-dir "/url/history")))
